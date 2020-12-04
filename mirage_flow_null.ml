type error = |
type write_error = [ `Closed ]

let pp_error _f = function (_:error) -> .

let pp_write_error = Mirage_flow.pp_write_error

type flow = unit

let read () = Lwt_result.return `Eof

let write () _buf = Lwt_result.return ()

let writev () _bufs = Lwt_result.return ()

let close () = Fmt.invalid_arg "close null!"
