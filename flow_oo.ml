open Lwt.Infix

type 'a ty = ..

type flow_error = ..

type error = flow_error * (Format.formatter -> unit)

type 'a or_error_lwt = ('a, [`Flow of error]) Lwt_result.t

class virtual reader = object (self)
  method virtual read_into : Cstruct.t -> [`Input of int | `Eof] or_error_lwt

  method read : [`Data of Cstruct.t | `Eof] or_error_lwt =
    let buf = Cstruct.create 4096 in
    self#read_into buf >|= function
    | Ok (`Input n) -> Ok (`Data (Cstruct.sub buf 0 n))
    | Ok `Eof | Error _ as x -> x
end

class virtual writer = object
  method virtual write : Cstruct.t -> unit or_error_lwt
end

class virtual flow = object
  inherit reader
  inherit writer
  method cast : 'a. 'a ty -> 'a option = fun _ -> None
end

let read_into (t:#reader) buf =
  (t#read_into buf : 'a or_error_lwt :> ('a, [> `Flow of error]) Lwt_result.t)

let read (t:#reader) =
  (t#read : 'a or_error_lwt :> ('a, [> `Flow of error]) Lwt_result.t)

let write (t:#writer) buf =
  (t#write buf : 'a or_error_lwt :> ('a, [> `Flow of error]) Lwt_result.t)

let cast (t:#flow) = t#cast

let null = object (_ : flow)
  inherit flow
  method! read = Lwt_result.return `Eof
  method read_into _buf = Lwt_result.return `Eof
  method write _buf = Lwt_result.return ()
end

let create_data () = object (_ : #flow)
  inherit flow

  val mutable read = 0
  val mutable wrote = 0
  val written = Cstruct.create (Cstruct.len Test_data.message)

  method read_into buf =
    let avail = Cstruct.len Test_data.message - read in
    let len = min (Cstruct.len buf) (min Test_data.chunk_size avail) in
    if len > 0 then (
      Cstruct.blit Test_data.message read buf 0 len;
      read <- read + len;
      Lwt_result.return (`Input len)
    ) else (
      Lwt_result.return `Eof
    )

  method! read =
    let avail = Cstruct.len Test_data.message - read in
    if avail > 0 then (
      let len = min Test_data.chunk_size avail in
      let chunk = Cstruct.sub Test_data.message read len in
      read <- read + len;
      Lwt_result.return (`Data chunk)
    ) else (
      Lwt_result.return `Eof
    )

  method write buf =
    let len = Cstruct.len buf in
    Cstruct.blit buf 0 written wrote len;
    wrote <- wrote + len;
    Lwt_result.return ()

  method close =
    assert (Cstruct.equal written Test_data.message)
end

let pp_error f (_, pp) = pp f

type flow_error += Msg

let failwith fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  (Msg, fun f -> Fmt.string f msg)
