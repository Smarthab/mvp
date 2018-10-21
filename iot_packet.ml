open Bin_prot.Std
open Huxiang

type t =
  { timestamp : int64; (* seconds since Epoch *)
    state     : bool   (* TODO: server/sensor unique id *)
  }
[@@deriving bin_io, show, eq]

let serialize (record : t) =
  let buf = Bin_prot.Utils.bin_dump bin_writer_t record in
  Utils.buffer_to_bytes buf

let deserialize (bytes : Bytes.t) =
  let buf = Utils.bytes_to_buffer bytes in
  bin_reader_t.read buf ~pos_ref:(ref 0)

let hash (record : t) =  
  Crypto.Hash.digest_bytes (serialize record)
