open Batteries

open Ocaml_geth
open Contract

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
  let%lwt receipt =
    Compile.deploy_rpc
      ~uri
      ~account
      ~contract:solidity_output
      ~arguments:[]
      ()
  in
  let receipt_str = Types.Tx.show_receipt receipt in
  Lwt_log.info_f "deploy: contract deployed; receipt:\n%s\n" receipt_str;%lwt
  Lwt.return (solidity_output, receipt)


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
      match find_method "log" with
      | None ->
        failwith "\"log\" method not found in solidity output"
      | Some abi -> abi
    in
    fun string start finish ->
      Lwt_log.debug "logging some stuff";%lwt
      let%lwt receipt =
        Compile.execute_method
          ~uri:P.uri
          ~abi:log_abi
          ~arguments:ABI.([ string_val string;
                            uint256_val start;
                            uint256_val finish ])
          ~src:P.account
          ~ctx:P.contract
          ()
      in
      Lwt.return (ABI.Decode.decode_events contract_abi.abi receipt)

end

