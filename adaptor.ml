open Lwt.Infix

type _ Conduit_oo.ty += Flow : Conduit_lwt.unpack Conduit_oo.ty

let of_pack (Conduit_lwt.Flow (t, (module F)) as pack) =
  let map_error x =
    x >|= function
    | Ok _ as x -> x
    | Error e -> Error (`Msg (Fmt.to_to_string F.pp_error e))
  in
  object (_ : Conduit_oo.flow)
    method recv buf = map_error (F.recv t buf)
    method send buf = map_error (F.send t buf)
    method close = map_error (F.close t)

    method cast : type a. a Conduit_oo.ty -> a option = function
      | Flow -> Some pack
      | _ -> None
  end

let null = of_pack (Conduit_lwt.Flow ((), (module Conduit_null.Null)))

let create_data () =
  Conduit_data.create_impl () >|= of_pack

let unwrap (t:#Conduit_oo.flow) = t#cast Flow
