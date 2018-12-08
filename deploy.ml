open Ocaml_geth

let read_file filename =
  let fd = open_in filename in
  Pervasives.input_line fd

let write_string_to_file filename =
  let fd = open_in filename in
  Pervasives.input_line fd

let main_lwt (solidity_file : string) (account : string) (uri : string) (secret : string) =
  let%lwt account =
    if Bitstr.Hex.is_hex account then
      Lwt.return (Bitstr.Hex.of_string account)
    else
      Lwt.fail_with @@ "Error: "^account^" is not a correctly formatted Eth address"
  in
  let passwd  = read_file secret in
  let%lwt unlock_success =
    Rpc.Personal.unlock_account ~uri ~account ~passphrase:passwd ~unlock_duration:60
  in
  if not unlock_success then
    (Lwt_log.debug_f "could not unlock account";%lwt
     Lwt.return None)
  else
    let%lwt _, receipt = EthLogger.deploy ~filename:solidity_file ~uri ~account in
    match receipt.Types.Tx.contract_address with
    | None ->
      Lwt_log.debug_f "deployment of contract %s failed" solidity_file;%lwt
      Lwt.return None
    | Some addr ->
      let addr = Bitstr.Hex.show addr in
      Lwt_log.debug_f "deploy: contract %s successfully deployed at address %s" solidity_file addr;%lwt
      Lwt.return (Some addr)

let main solidity_file account uri secret =
  let result = Lwt_main.run (main_lwt solidity_file account uri secret) in
  match result with
  | None -> ()
  | Some res ->
    print_string res;
    flush_all ()
    
open Cmdliner

let solidity_file =
  let doc = "Solidity contract file." in
  Arg.(required & opt (some string) None & info ["solidity"] ~doc)

let secret_file =
  let doc = "File containing passphrase for the paying account." in
  Arg.(required & opt (some string) None & info ["secret"] ~doc)

let uri =
  let doc = "Uri of rpc interface. Defaults to http://localhost:8545" in
  Arg.(value & opt string "http://localhost:8545" & info ["uri"] ~doc)

let account =
  let doc = "Ethereum account paying for logging (0x-prefixed hexadecimal)" in
  Arg.(required & opt (some string) None & info ["account"] ~doc)

let info =
  let doc = "Deploys a solidity contract." in
  let man = [
    `S Manpage.s_bugs;
    `P "Report bugs on https://github.com/SmartHab/mvp" ]
  in
  Term.info "deploy" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

let main_term =
  Term.(const main $ solidity_file $ account $ uri $ secret_file)


let () = Term.exit @@ Term.eval (main_term, info)
