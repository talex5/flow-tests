type error = |
type write_error = [ `Closed ]

let pp_error _f = function (_:error) -> .

let pp_write_error = Mirage_flow.pp_write_error

type flow = {
  mutable read : int;
  mutable wrote : int;
  written : Cstruct.t;
}

let read t =
  let avail = Cstruct.len Test_data.message - t.read in
  if avail > 0 then (
    let len = min Test_data.chunk_size avail in
    let chunk = Cstruct.sub Test_data.message t.read len in
    t.read <- t.read + len;
    Lwt_result.return (`Data chunk)
  ) else (
    Lwt_result.return `Eof
  )

let write t buf =
  let len = Cstruct.len buf in
  Cstruct.blit buf 0 t.written t.wrote len;
  t.wrote <- t.wrote + len;
  Lwt_result.return ()

let writev _t _bufs = failwith "TODO"

let close t =
  assert (Cstruct.equal t.written Test_data.message);
  Lwt.return_unit

let create () =
  { read = 0; wrote = 0; written = Cstruct.create (Cstruct.len Test_data.message) }
