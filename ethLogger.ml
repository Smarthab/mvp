open Batteries

open Ocaml_geth
open Contract


(* --------------------------------------------------------------------- *)
(* Some helpful functions *)
(* --------------------------------------------------------------------- *)

(* Read password from stdin in a stealthy manner *)
let read_secret () =
  let open Unix in
  let term_init = tcgetattr stdin in
  let term_no_echo = { term_init with c_echo = false } in
  tcsetattr stdin TCSADRAIN term_no_echo;
  let password =
    try read_line ()
    with _ ->
      (tcsetattr stdin TCSAFLUSH term_init;
       failwith "read_secret: readline failed")
  in
  tcsetattr stdin TCSAFLUSH term_init;
  password

let input_password (account : Types.Address.t) =
  Printf.printf "password for account %s: %!" (account :> string);
  let res = read_secret () in
  print_newline ();
  res


(* --------------------------------------------------------------------- *)
(* Deploying a smart contract. We functorize the code over some global
   parameters (like the creating account). *)
(* --------------------------------------------------------------------- *)

let deploy    
    ~filename (* solidity file *)
    ~uri      (* uri of the Geth node *)
    ~account  (* account paying for deployment -- we assume that it is unlocked *)
  =
  let solidity_output = Compile.to_json ~filename in
  let deployment_gas  = Z.of_int 500000 in (* TODO: estimate via rpc *)
  let receipt =
    Compile.deploy_rpc
      ~uri
      ~account
      ~gas:deployment_gas
      ~contract:solidity_output
      ~arguments:[]
      ~value:None
  in
  if Z.equal receipt.Types.Tx.gas_used deployment_gas then
    failwith "deploy: contract could not be deployed"
  else
    let receipt_str = Types.Tx.show_receipt receipt in
    Printf.printf "deploy: contract deployed; receipt:\n%s\n" receipt_str;
    (solidity_output, receipt)


module type Logger =
sig
  (* A Logger provides a [log] function, taking as input a sequence of 
     bytes of arbitrary length and a pair of timestamps. The [log] 
     function performs a method call on the Logger contract it
     is attached to and returns the list of any event triggered during
     execution of said contract.
  *)
  val log : string -> Int64.t -> Int64.t -> ABI.event list Lwt.t
end

module type Params =
sig

  (* Path to solidity file *)
  val filename : string

  (* uri of the Geth node *)
  val uri : string

  (* Account paying for transactions -- we assume that it is unlocked *)
  val account : Types.Address.t

  (* Contract address *)
  val contract : Types.Address.t
  
end

module Make(P : Params) =
struct

  let solidity_output = Compile.to_json ~filename:P.filename

  let contract_abi =
    match solidity_output.contracts with
    | [] | _ :: _ :: _ ->
      failwith "Storage: more than one contract"
    | [ctx] -> ctx

  let find_method mname =
    Compile.get_method contract_abi mname

  let log =
    let log_abi =
      match find_method "log"  with
      | None -> failwith "log method not found in solidity output"
      | Some abi -> abi
    in
    fun string start finish ->
      let given_gas = Z.of_int 99999 in
      let%lwt receipt =
        Compile.execute_method_lwt
          ~uri:P.uri
          ~abi:log_abi
          ~arguments:ABI.([ string_val string;
                            uint256_val start;
                            uint256_val finish ])
          ~src:P.account
          ~ctx:P.contract
          ~gas:(Z.of_int 99999)
          ~value:None
      in
      if Z.equal receipt.Types.Tx.gas_used given_gas then
        let msg =
          "service_keepalive: contract call terminated in an erroneous state"
        in
        Lwt.fail_with msg
      else
        Lwt.return (ABI.Decode.decode_events contract_abi.abi receipt)

end

