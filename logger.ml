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

(* module LoggerProcess(X : ArgSig) =
 * struct
 * 
 *   open Huxiang
 * 
 *   module M = Mother(X)
 *   
 *   module I =
 *   struct
 *     type t = 
 *       | Logg of Messages.BrokerToMother.t
 *       | FromService of Messages.ServiceToMother.t
 *     [@@deriving eq, show, bin_io]
 *   end
 * 
 *   module O = Messages.Nothing
 * 
 *   type input  = I.t
 *   type output = O.t Address.multi_dest
 * 
 *   type state = unit [@@deriving show]
 * 
 *   let name = Name.atom "logger"
 * 
 *   let fail where msg =
 *     Lwt.fail_with @@ where^ ": "^msg
 * 
 *   let rec main_loop state =
 *     let fname = "mother/motherprocess/main_loop" in
 *     Process.with_input (function
 *         | I.FromBroker Deposit ->
 *           Lwt.ignore_result (M.broker_deposit (Z.of_int 1000));
 *           Lwt_io.printf "broker: deposit successful\n";%lwt
 *           Process.continue_with state main_loop
 *         | I.FromService Deposit ->
 *           Lwt.ignore_result (M.service_deposit (Z.of_int 1000));
 *           Lwt_io.printf "service: deposit successful\n";%lwt
 *           Process.continue_with state main_loop
 *         | I.FromBroker KeepAlive ->
 *           let%lwt events = M.broker_keepalive () in
 *           (match events with
 *            | [] ->
 *              let msg = "no messages written when calling broker_keepalive" in
 *              fail fname msg
 *            | _ :: _ :: _ ->
 *              let msg =
 *                "more that one message written when calling broker_keepalive"
 *              in
 *              fail fname msg
 *            | [{ event_name; event_args }] ->
 *              (match event_name, event_args with
 *               | "ServiceTimeout", [ { desc = ABI.Int delta; _ } ] ->
 *                 let msg = 
 *                   Printf.sprintf
 *                     "service timeout (%Ld) received from mother contract" 
 *                     delta 
 *                 in
 *                 fail fname msg
 *               | "ServiceAlive", [ { desc = ABI.Int delta; _ } ]  ->
 *                 Lwt_io.printf "broker: keepalive successful (%Ld)\n" delta;%lwt
 *                 Process.continue_with state main_loop
 *               | _ ->
 *                 fail fname ("unknown event "^event_name)
 *              )
 *           )
 *         | I.FromService KeepAlive ->
 *           let%lwt events = M.service_keepalive () in
 *           (match events with
 *            | [] ->
 *              let msg =
 *                "no messages written when calling service_keepalive"
 *              in
 *              fail fname msg
 *            | _ :: _ :: _ ->
 *              let msg =
 *                "more that one message written when calling service_keepalive"
 *              in
 *              fail fname msg
 *            | [{ event_name; event_args }] ->
 *              (match event_name, event_args with
 *               | "BrokerTimeout", [ { desc = ABI.Int delta; _ } ] ->
 *                 let msg =
 *                   Printf.sprintf
 *                     "broker timeout (%Ld) received from mother contract" 
 *                     delta 
 *                 in
 *                 fail fname msg
 *               | "BrokerAlive",  [ { desc = ABI.Int delta; _ } ] ->
 *                 Lwt_io.printf "service: keepalive successful (%Ld)\n" delta;%lwt
 *                 Process.continue_with state main_loop
 *               | _ ->
 *                 fail fname ("unknown event "^event_name)
 *              )
 *           )
 *       )
 * 
 *   let thread =
 *     {
 *       Process.move  = main_loop;
 *       Process.state = ()
 *     }
 *   
 * end
 * 
 * (\* for simplicity, deploy the mother contract with the same account that plays
 *    as "service". In reality, this could be e.g. a "smarthab" account, with
 *    payement done either on-chain or in fiat by the parties. *\)
 * module Mom =
 *   MotherProcess(struct 
 *     let account = 
 *       Types.Address.from_string "0xa742250854eeabb8a4165ee5fd1f5f76c69bf053"
 *     let service = 
 *       Types.Address.from_string "0xa742250854eeabb8a4165ee5fd1f5f76c69bf053"
 *     let broker  = 
 *       Types.Address.from_string "0x7beb9484e091bccd84ffa9a1fda0ba59092c1f79"
 *     let timeout = 10L
 *     let uri     = "http://localhost:8545"
 *   end)
 * 
 * open Huxiang
 * 
 * let compiled_mom =
 *   NetProcess.compile (function
 *       | None ->
 *         failwith "no public key"
 *       | Some pkey ->
 *         if Crypto.Public.equal pkey Directory.BrokerCred.public_key then
 *           let inject x = Mom.I.FromBroker x in
 *           Utils.map_reader Messages.BrokerToMother.bin_reader_t inject
 *         else if Crypto.Public.equal pkey Directory.ServiceCred.public_key then
 *           let inject x = Mom.I.FromService x in
 *           Utils.map_reader Messages.ServiceToMother.bin_reader_t inject
 *         else
 *           failwith "unknown public key"
 *     ) Messages.Nothing.bin_writer_t (module Mom)
 * 
 * module MomCred = (val Crypto.(key_pair_to_cred (seeded_key_pair "mother")))
 * 
 * let _ =
 *   Ocaml_geth.Rpc.switch_debug ();
 *   let module MomNode = Huxiang.Node.Make((val compiled_mom)) in
 *   let () = Lwt_log.add_rule "*" Lwt_log.Debug in
 *   Lwt_log.default := (Lwt_log.channel
 *                         ~template:"[$(level)] $(message)"
 *                         ~channel:Lwt_io.stderr
 *                         ~close_mode:`Keep
 *                         ());
 *   MomNode.start
 *     ~listening:Directory.mother
 *     ~network_map:Directory.network_map
 *     ~skey:Directory.MomCred.secret_key
 *     ~pkey:Directory.MomCred.public_key *)

