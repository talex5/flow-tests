type 'a ty = ..

class virtual reader : object
  method virtual read_into : Cstruct.t -> (int, [`Eof | `Msg of string ]) Lwt_result.t
  method read : (Cstruct.t, [`Eof | `Msg of string ]) Lwt_result.t
end

class virtual writer : object
  method virtual write : Cstruct.t -> (unit, [`Msg of string ]) Lwt_result.t 
end

class virtual flow : object
  inherit reader
  inherit writer
  method virtual close : (unit, [`Msg of string ]) Lwt_result.t
  method cast : 'a. 'a ty -> 'a option
end

val read_into : #reader -> Cstruct.t -> (int, [> `Eof | `Msg of string ]) Lwt_result.t
val read      : #reader -> (Cstruct.t, [> `Eof | `Msg of string ]) Lwt_result.t
val write     : #writer -> Cstruct.t -> (unit, [> `Msg of string ]) Lwt_result.t
val close     : #flow -> (unit, [> `Msg of string ]) Lwt_result.t
val cast      : #flow -> 'a ty -> 'a option

val null : flow

(* Test data *)
val create_data : unit -> flow
