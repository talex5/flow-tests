open Lwt.Infix
open Core_bench

(* Test case: Read from a flow, echoing the results back to that flow. *)

let buf = Cstruct.create 4096

let ( >>!= ) = Lwt_result.bind

(* Approach 1 : Mirage_flow *)

module Test(F : Mirage_flow.S) = struct
  let rec copy flow =
    F.read flow >>= function
    | Ok `Eof -> Lwt_result.return ()
    | Error e -> Lwt_result.fail (`Read e)
    | Ok (`Data buf) ->
      F.write flow buf >>= function
      | Error e -> Lwt_result.fail (`Write e)
      | Ok () -> copy flow

  let test flow =
    copy flow >|= function
    | Ok () -> ()
    | Error (`Read e) -> Fmt.failwith "%a" F.pp_error e
    | Error (`Write e) -> Fmt.failwith "%a" F.pp_write_error e
end

module Test_mirage_flow_null = Test(Mirage_flow_null)
module Test_mirage_flow_data = Test(Mirage_flow_data)

(* Approach 2 : Conduit 3 *)

let conduit_null =
  Lwt_main.run begin
    Conduit_lwt.connect () Conduit_null.t >|= function
    | Error (`Msg m) -> failwith m
    | Error `Not_found -> failwith "Not found"
    | Ok x -> x
  end

let rec copy_conduit flow =
  Conduit_lwt.recv flow buf >>!= function
  | `End_of_flow -> Lwt_result.return ()
  | `Input n ->
    let rec aux i =
      if i = n then copy_conduit flow
      else (
        Conduit_lwt.send flow (Cstruct.sub buf i (n - i)) >>!= fun j ->
        aux (i + j)
      )
    in
    aux 0

let test_conduit flow =
  copy_conduit flow >|= function
  | Ok () -> ()
  | Error `Not_found -> assert false   (* Can't happen *)
  | Error (`Msg m) -> failwith m

let copy_conduit_impl (Conduit_lwt.Flow (flow, (module F))) =
  let rec loop () =
    F.recv flow buf >>!= function
    | `End_of_flow -> Lwt_result.return ()
    | `Input n ->
      let rec aux i =
        if i = n then loop ()
        else (
          F.send flow (Cstruct.sub buf i (n - i)) >>!= fun j ->
          aux (i + j)
        )
      in
      aux 0
  in
  loop () >|= function
  | Ok () -> Ok ()
  | Error e -> Error (`Msg (Fmt.to_to_string F.pp_error e))

(* Wrapping a Conduit FLOW with an object *)
let test_conduit_impl pack =
  copy_conduit_impl pack >|= function
  | Ok () -> ()
  | Error (`Msg m) -> failwith m

(* Approach 3 : An OO interface based on Conduit 3's API *)

let rec copy_conduit_oo flow =
  Conduit_oo.recv flow buf >>!= function
  | `End_of_flow -> Lwt_result.return ()
  | `Input n ->
    let rec aux i =
      if i = n then copy_conduit_oo flow
      else (
        Conduit_oo.send flow (Cstruct.sub buf i (n - i)) >>!= fun j ->
        aux (i + j)
      )
    in
    aux 0

let test_conduit_oo flow =
  copy_conduit_oo flow >|= function
  | Ok () -> ()
  | Error (`Msg m) -> failwith m

(* Approach 4 : Another OO interface *)

let rec copy_flow_oo flow =
  Flow_oo.read_into flow buf >>!= function
  | `Eof -> Lwt_result.return ()
  | `Input n ->
    Flow_oo.write flow (Cstruct.sub buf 0 n) >>!= fun () ->
    copy_flow_oo flow

let test_flow_oo flow =
  copy_flow_oo flow >|= function
  | Ok () -> ()
  | Error (`Flow e) -> Fmt.failwith "%a" Flow_oo.pp_error e

(* Approach 5 : Using the read call for zero-copy *)

let rec copy_flow_oo2 flow =
  Flow_oo.read flow >>!= function
  | `Eof -> Lwt_result.return ()
  | `Data buf ->
    Flow_oo.write flow buf >>!= fun () ->
    copy_flow_oo2 flow

let test_flow_oo2 flow =
  copy_flow_oo2 flow >|= function
  | Ok () -> ()
  | Error (`Flow e) -> Fmt.failwith "%a" Flow_oo.pp_error e

(* Check they work *)

let () =
  Lwt_main.run begin
    (* Mirage flow *)
    print_endline "Mirage_flow_data";
    let flow = Mirage_flow_data.create () in
    Test_mirage_flow_data.test flow >>= fun () ->
    Mirage_flow_data.close flow >>= fun () ->
    (* Conduit *)
    print_endline "Conduit";
    Conduit_data.create () >>= fun flow ->
    test_conduit flow >>= fun () ->
    Conduit_lwt.close flow >|= Result.get_ok >>= fun () ->
    print_endline "Conduit impl";
    Conduit_data.create_impl () >>= fun pack ->
    test_conduit_impl pack >>= fun () ->
    let Conduit_lwt.Flow (flow, (module F)) = pack in
    F.close flow >|= Result.get_ok >>= fun () ->
    (* Conduit OO *)
    print_endline "Conduit_oo";
    let flow = Conduit_oo.create_data () in
    test_conduit_oo flow >>= fun () ->
    Conduit_oo.close flow >|= Result.get_ok >>= fun () ->
    print_endline "adaptor";
    Adaptor.create_data () >>= fun flow ->
    test_conduit_oo flow >>= fun () ->
    Conduit_oo.close flow >|= Result.get_ok >>= fun () ->
    print_endline "adaptor_unwrap";
    Adaptor.create_data () >>= fun flow ->
    Adaptor.unwrap flow |> Option.get |> test_conduit_impl >>= fun () ->
    Conduit_oo.close flow >|= Result.get_ok >>= fun () ->
    (* Flow OO *)
    print_endline "Flow_oo";
    let flow = Flow_oo.create_data () in
    test_flow_oo flow >>= fun () ->
    flow#close;
    print_endline "Flow_oo2";
    let flow = Flow_oo.create_data () in
    test_flow_oo2 flow >>= fun () ->
    flow#close;
    Lwt.return_unit
  end

(* Benchmark *)

let () =
  Core.Command.run (Bench.make_command [
      Bench.Test.create ~name:"mirage_flow_null" (fun () -> Test_mirage_flow_null.test ());
      Bench.Test.create ~name:"conduit_null" (fun () -> test_conduit conduit_null);
      Bench.Test.create ~name:"conduit_oo_null" (fun () -> test_conduit_oo Conduit_oo.null);
      Bench.Test.create ~name:"adaptor_null" (fun () -> test_conduit_oo Adaptor.null);
      Bench.Test.create ~name:"adaptor_null_unwrap" (fun () -> test_conduit_impl (Option.get (Adaptor.unwrap Adaptor.null)));
      Bench.Test.create ~name:"oo_null" (fun () -> test_flow_oo Flow_oo.null);
      Bench.Test.create ~name:"oo_null2" (fun () -> test_flow_oo2 Flow_oo.null);

      Bench.Test.create ~name:"mirage_flow_data" (fun () -> Test_mirage_flow_data.test (Mirage_flow_data.create ()));
      Bench.Test.create ~name:"conduit_data" (fun () -> Conduit_data.create () >>= test_conduit);
      Bench.Test.create ~name:"conduit_data_unwrap" (fun () -> Conduit_data.create_impl () >>= test_conduit_impl);
      Bench.Test.create ~name:"conduit_oo_data" (fun () -> test_conduit_oo (Conduit_oo.create_data ()));
      Bench.Test.create ~name:"adaptor_data" (fun () -> Adaptor.create_data () >>= test_conduit_oo);
      Bench.Test.create ~name:"adaptor_data_unwrap" (fun () -> Adaptor.create_data () >|= Adaptor.unwrap >|= Option.get >>= test_conduit_impl);
      Bench.Test.create ~name:"oo_data" (fun () -> test_flow_oo (Flow_oo.create_data ()));
      Bench.Test.create ~name:"oo_data2" (fun () -> test_flow_oo2 (Flow_oo.create_data ()));
    ])
