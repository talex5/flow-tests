type 'a ty = ..

class type flow = object
  method recv : Cstruct.t -> ([ `Input of int | `End_of_flow ], [> `Msg of string ]) Lwt_result.t
  method send : Cstruct.t -> (int, [> `Msg of string ]) Lwt_result.t
  method close : (unit, [> `Msg of string ]) Lwt_result.t
  method cast : 'a. 'a ty -> 'a option
end

let recv (t:#flow) = t#recv
let send (t:#flow) = t#send
let close (t:#flow) = t#close
let cast (t:#flow) = t#cast

let null = object (_ : flow)
  method recv _buf = Lwt_result.return `End_of_flow
  method send buf = Lwt_result.return (Cstruct.len buf)
  method close = Fmt.invalid_arg "close null!"
  method cast _ = None
end

let create_data () = object (_ : flow)
  val mutable read = 0
  val mutable wrote = 0
  val written = Cstruct.create (Cstruct.len Test_data.message)

  method recv buf =
    let avail = Cstruct.len Test_data.message - read in
    let len = min (Cstruct.len buf) (min Test_data.chunk_size avail) in
    if len > 0 then (
      Cstruct.blit Test_data.message read buf 0 len;
      read <- read + len;
      Lwt_result.return (`Input len)
    ) else (
      Lwt_result.return `End_of_flow
    )

  method send buf =
    let len = Cstruct.len buf in
    Cstruct.blit buf 0 written wrote len;
    wrote <- wrote + len;
    Lwt_result.return len

  method close =
    assert (Cstruct.equal written Test_data.message);
    Lwt_result.return ()

  method cast _ = None
end
