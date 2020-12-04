type 'a ty = ..

class virtual reader = object (self)
  method virtual read_into : Cstruct.t -> (int, [`Eof | `Msg of string ]) Lwt_result.t
  method read =
    let buf = Cstruct.create 4096 in
    self#read_into buf |> Lwt_result.map (fun n -> Cstruct.sub buf 0 n)
end

class virtual writer = object
  method virtual write : Cstruct.t -> (unit, [`Msg of string ]) Lwt_result.t 
end

class virtual flow = object
  inherit reader
  inherit writer
  method virtual close : (unit, [`Msg of string ]) Lwt_result.t
  method cast : 'a. 'a ty -> 'a option = fun _ -> None
end

let read_into (t:#reader) buf =
  (t#read_into buf
   : (int, [`Eof | `Msg of string ]) Lwt_result.t
   :> (int, [> `Eof | `Msg of string ]) Lwt_result.t)

let read (t:#reader) =
  (t#read
   : (Cstruct.t, [`Eof | `Msg of string ]) Lwt_result.t
   :> (Cstruct.t, [> `Eof | `Msg of string ]) Lwt_result.t)

let write (t:#writer) buf =
  (t#write buf
   : (unit, [`Msg of string ]) Lwt_result.t
   :> (unit, [> `Msg of string ]) Lwt_result.t)

let close (t:#flow) =
  (t#close
   : (unit, [`Msg of string ]) Lwt_result.t
   :> (unit, [> `Msg of string ]) Lwt_result.t)

let cast (t:#flow) = t#cast

let null = object (_ : flow)
  inherit flow
  method! read = Lwt_result.fail `Eof
  method read_into _buf = Lwt_result.fail `Eof
  method write _buf = Lwt_result.return ()
  method close = Fmt.invalid_arg "close null!"
end

let create_data () = object (_ : flow)
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
      Lwt_result.return len
    ) else (
      Lwt_result.fail `Eof
    )

  method! read =
    let avail = Cstruct.len Test_data.message - read in
    if avail > 0 then (
      let len = min Test_data.chunk_size avail in
      let chunk = Cstruct.sub Test_data.message read len in
      read <- read + len;
      Lwt_result.return chunk
    ) else (
      Lwt_result.fail `Eof
    )

  method write buf =
    let len = Cstruct.len buf in
    Cstruct.blit buf 0 written wrote len;
    wrote <- wrote + len;
    Lwt_result.return ()

  method close =
    assert (Cstruct.equal written Test_data.message);
    Lwt_result.return ()
end
