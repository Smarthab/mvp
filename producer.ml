open Bin_prot.Std
open Huxiang

let time_microsec () =
  let t = Unix.gettimeofday () in
  let t = t *. 1000000.0 in
  Int64.of_float t

module Producer =
struct

  module I =
  struct

    type t = unit
    [@@deriving bin_io]

  end

  module O =
  struct

    type t = Iot_packet.t
    [@@deriving bin_io]

  end

  type input  = I.t
  type output = O.t Address.multi_dest

  let name = Name.atom "producer"

  type state = { rate : float } (* rate of Poisson process *)
  [@@deriving show]

  let rec process state =
    Process.without_input
      begin
        let t = ~-. (log (Random.float 1.0)) /. state.rate in
        Lwt_unix.sleep t;%lwt
        let now    = time_microsec () in
        let output =
          (* empty dest list = broadcast *)
          Address.({ Iot_packet.timestamp = now; state = Random.bool () } @+ [])
        in
        Process.continue_with ~output state process
      end

  let thread =
    {
      Process.state = { rate = 1.0 };
      move          = process
    }

end

let compiled_producer :> (module NetProcess.S) = 
  NetProcess.compile (fun _ -> Producer.I.bin_reader_t) Producer.O.bin_writer_t (module Producer)

module Server = Huxiang.Node.Make((val compiled_producer))

let main publish_addr secret_phrase_file =
  let secret_phrase =
    let fd = open_in secret_phrase_file in
    let res = input_line fd in
    close_in fd;
    res
  in
  let credentials = Crypto.(key_pair_to_cred (seeded_key_pair secret_phrase)) in
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  Server.start
    ~listening:[]
    ~network_map:(fun _ -> Publish publish_addr)
    ~credentials


open Cmdliner

let publish_addr =
  let doc = "Address on which to publish stream." in
  Arg.(required & opt (some string) None & info ["addr"] ~doc)

let secret_phrase =
  let doc = "File containing secret phrase for generating public/secret key pair." in
  Arg.(required & opt (some string) None & info ["secret"] ~doc)

let producer_term =
  Term.(const main $ publish_addr $ secret_phrase)

let info =
  let doc = "Runs a node producing data" in
  let man = [
    `S Manpage.s_bugs;
    `P "Report bugs on https://github.com/SmartHab/mvp" ]
  in
  Term.info "producer" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (producer_term, info)
