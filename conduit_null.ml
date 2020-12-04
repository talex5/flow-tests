module Null = struct
  type 'a io = 'a Lwt.t

  type flow = unit
  type input = Cstruct.t
  type output = Cstruct.t
  type error = |

  let pp_error _f = function (_:error) -> .

  let recv () _buf = Lwt_result.return `End_of_flow

  let send () buf = Lwt_result.return (Cstruct.len buf)

  let close () = Fmt.invalid_arg "close null!"

  type endpoint = unit

  let connect () = Lwt_result.return ()
end

let t = Conduit_lwt.register ~protocol:(module Null)
include (val (Conduit_lwt.repr t))
