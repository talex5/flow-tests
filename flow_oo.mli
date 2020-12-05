type 'a ty = ..

type flow_error = ..
type flow_error += Msg

type error = flow_error * (Format.formatter -> unit)

type 'a or_error_lwt = ('a, [`Flow of error]) Lwt_result.t

class virtual reader : object
  method virtual read_into : Cstruct.t -> [`Input of int | `Eof] or_error_lwt
  method read : [`Data of Cstruct.t | `Eof] or_error_lwt
end

class virtual writer : object
  method virtual write : Cstruct.t -> unit or_error_lwt
end

class virtual flow : object
  inherit reader
  inherit writer
  method cast : 'a. 'a ty -> 'a option
end

val read_into : #reader -> Cstruct.t -> ([`Input of int | `Eof], [> `Flow of error]) Lwt_result.t
val read      : #reader -> ([`Data of Cstruct.t | `Eof], [> `Flow of error]) Lwt_result.t
val write     : #writer -> Cstruct.t -> (unit, [> `Flow of error]) Lwt_result.t
val cast      : #flow -> 'a ty -> 'a option

val null : flow

val pp_error : error Fmt.t

val failwith : ('a, Format.formatter, unit, error) format4 -> 'a

val create_data : unit -> < flow; close : unit >
