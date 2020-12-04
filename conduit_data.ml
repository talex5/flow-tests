open Lwt.Infix

module Data = struct
  type 'a io = 'a Lwt.t

  type flow = {
    mutable read : int;
    mutable wrote : int;
    written : Cstruct.t;
  }

  type input = Cstruct.t
  type output = Cstruct.t
  type error = |

  let pp_error _f = function (_:error) -> .

  let recv t buf =
    let avail = Cstruct.len Test_data.message - t.read in
    let len = min (Cstruct.len buf) (min Test_data.chunk_size avail) in
    if len > 0 then (
      Cstruct.blit Test_data.message t.read buf 0 len;
      t.read <- t.read + len;
      Lwt_result.return (`Input len)
    ) else (
      Lwt_result.return `End_of_flow
    )

  let send t buf =
    let len = Cstruct.len buf in
    Cstruct.blit buf 0 t.written t.wrote len;
    t.wrote <- t.wrote + len;
    Lwt_result.return len

  let close t =
    assert (Cstruct.equal t.written Test_data.message);
    Lwt_result.return ()

  type endpoint = unit

  let connect () =
    let t = { read = 0; wrote = 0; written = Cstruct.create (Cstruct.len Test_data.message) } in
    Lwt_result.return t
end

let t = Conduit_lwt.register ~protocol:(module Data)
include (val (Conduit_lwt.repr t))

let create () =
  Conduit_lwt.connect () t >|= function
  | Error (`Msg m) -> failwith m
  | Error `Not_found -> failwith "Not found"
  | Ok x -> x
