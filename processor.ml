open Batteries
open Bin_prot.Std
open Huxiang
open Ocaml_geth

type block = Iot_packet.t list
[@@deriving bin_io]

let open_database db_name = 
  Dbm.opendbm db_name [Dbm.Dbm_create; Dbm.Dbm_rdwr] 0o660

let interval_of_packet_list (block : Iot_packet.t list) =
  let timestamps = 
    List.map (fun { Iot_packet.timestamp; _ } -> timestamp) block
  in
  let timestamps = List.sort Int64.compare timestamps in
  let fst = List.hd timestamps in
  let lst = List.hd (List.rev timestamps) in
  (fst, lst)

let block_to_bytes (block : Iot_packet.t list) =
  let buf = Bin_prot.Utils.bin_dump bin_writer_block block in
  Utils.buffer_to_bytes buf

module type ParamsS =
sig
  (* database *)
  val db       : Dbm.t

  (* max # of packets in block *)
  val max_size : int

  (* Logger.make parameters *)
  val logger_params : (module EthLogger.Params)

  (* Password for the paying account *)
  val password : string
end

module Make(Params : ParamsS) =
struct

  module I =
  struct
    
    type t = Iot_packet.t
    [@@deriving bin_io, eq, show]

  end

  module O =
  struct
    
    type t = unit
    [@@deriving bin_io, eq, show]

  end

  type input  = I.t
  type output = O.t Address.multi_dest

  let name = Name.atom "processor"

  type state =
    {
      (* internal buffer of not-yet-written IOT packets *)
      buffer  : I.t list;
      (* max size of internal buffer *)
      max_buf : int
    }
  [@@deriving show]

  (* Instantiate module to write on the chain *)
  module LoggerParams = (val Params.logger_params)
  module Logger       = EthLogger.Make(LoggerParams)
  
  let write_block string start finish =
    Rpc.Personal.unlock_account
      ~account:LoggerParams.account
      ~uri:LoggerParams.uri
      ~passphrase:Params.password
      ~unlock_duration:60;
    let%lwt events = Logger.log string start finish in
    let successful =
      List.exists (function
          | Contract.ABI.{ event_name = "BlockWritten"; _ } -> true;
          | _ -> false
        ) events
    in
    Lwt.return successful
    
  let rec process state =
    Process.with_input (fun packet ->
        Lwt_log.info_f "received packet %s" (I.show packet);%lwt
        let state = { state with buffer = packet :: state.buffer } in
        if List.length state.buffer >= state.max_buf then
          let block = state.buffer in
          let state = { state with buffer = [] } in
          let bytes = block_to_bytes block in
          let hash  = Crypto.Hash.digest_bytes bytes in
          let key   = hash |> Crypto.Hash.to_bytes |> Bytes.to_string in
          Dbm.add Params.db key (Bytes.to_string bytes);
          let key_hex = Hex.of_string key in
          let start, finish = interval_of_packet_list block in
          let%lwt result = write_block key start finish in
          if result then
            (Lwt_log.info_f
               "wrote block of length %d with hash %s" 
               (List.length block)
               (Hex.show key_hex);%lwt
             Process.continue_with state process)
          else
            Process.stop state
        else
          Process.continue_with state process
      )

  let thread =
    {
      Process.state = { buffer = []; 
                        max_buf = Params.max_size };
      move          = process
    }
  
end


let main 
    (listen_addr : string)
    (secret_phrase : string)
    (db_name : string)
    (solidity_file : string)
    (uri : string)
    (account : string)
    (contract_addr : string) =
  let module Lprm : EthLogger.Params =
  struct
    let filename = solidity_file
    let uri      = uri
    let account  = Bitstr.Hex.of_string account
    let contract = Bitstr.Hex.of_string contract_addr
  end
  in
  let module Params : ParamsS =
  struct
    let db       = open_database db_name
    let max_size = 10
    let logger_params = (module Lprm : EthLogger.Params)
    let password = EthLogger.input_password Lprm.account
  end
  in
  let module Processor = Make(Params) in
  let compiled_processor = 
    NetProcess.compile 
      (fun _ -> Processor.I.bin_reader_t) 
      Processor.O.bin_writer_t 
      (module Processor)
  in
  let module Server = Huxiang.Node.Make((val compiled_processor)) in
  let credentials = Crypto.(key_pair_to_cred (seeded_key_pair secret_phrase)) in
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  Server.start
    ~listening:[Subscribe listen_addr]
    ~network_map:(fun _ -> failwith "network map should never be called")
    ~credentials

open Cmdliner

(* options *)

let listen_addr =
  let doc = "Address at which we listen for a stream of packets." in
  Arg.(required & opt (some string) None & info ["listen"] ~doc)

let secret_phrase =
  let doc = "Secret phrase for generating public/secret key pair." in
  Arg.(required & opt (some string) None & info ["secret"] ~doc)

let db_file =
  let doc = "Name of the database." in
  Arg.(required & opt (some string) None & info ["dbfile"] ~doc)

let solidity_file =
  let doc = "Solidity contract." in
  Arg.(required & opt (some string) None & info ["solidity"] ~doc)

let uri =
  let doc = "uri of Geth node. Defaults to http://localhost:8545" in
  Arg.(value & opt string "http://localhost:8545" & info ["uri"] ~doc)

let account =
  let doc = "Ethereum account paying for logging (0x-prefixed hexadecimal)" in
  Arg.(required & opt (some string) None & info ["account"] ~doc)

let contract_addr =
  let doc = "Address of logging contract (0x-prefixed hexadecimal)" in
  Arg.(required & opt (some string) None & info ["contract"] ~doc)

let processor_term =
  Term.(const main $ listen_addr $ secret_phrase $ db_file $ solidity_file $ uri $ account $ contract_addr)

let info =
  let doc = "Runs a node processing data" in
  let man = [
    `S Manpage.s_bugs;
    `P "Report bugs on https://github.com/SmartHab/mvp" ]
  in
  Term.info "processor" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (processor_term, info)
