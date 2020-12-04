open Lwt.Infix
open Core_bench

(* Test case: Read from a flow, echoing the results back to that flow. *)

let buf = Cstruct.create 4096

(* Approach 1 : Mirage_flow *)

module Test(F : Mirage_flow.S) = struct
  let rec test flow =
    F.read flow >>= function
    | Ok `Eof -> Lwt.return_unit
    | Error e -> Fmt.failwith "%a" F.pp_error e
    | Ok (`Data buf) ->
      F.write flow buf >>= function
      | Error e -> Fmt.failwith "%a" F.pp_write_error e
      | Ok () -> test flow
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

let rec test_conduit flow =
  Conduit_lwt.recv flow buf >>= function
  | Error (`Msg m) -> failwith m
  | Error `Not_found -> assert false    (* ?? *)
  | Ok `End_of_flow -> Lwt.return_unit
  | Ok (`Input n) ->
    let rec aux i =
      if i = n then test_conduit flow
      else (
        Conduit_lwt.send flow (Cstruct.sub buf i (n - i)) >>= function
        | Ok j -> aux (i + j)
        | Error `Not_found -> assert false    (* ?? *)
        | Error (`Msg m) -> failwith m
      )
    in
    aux 0

(* Approach 3 : An OO interface based on Conduit 3's API *)

let rec test_conduit_oo flow =
  Conduit_oo.recv flow buf >>= function
  | Ok `End_of_flow -> Lwt.return_unit
  | Error (`Msg m) -> failwith m
  | Ok (`Input n) ->
    let rec aux i =
      if i = n then test_conduit_oo flow
      else (
        Conduit_oo.send flow (Cstruct.sub buf i (n - i)) >>= function
        | Ok j -> aux (i + j)
        | Error `Not_found -> assert false    (* ?? *)
        | Error (`Msg m) -> failwith m
      )
    in
    aux 0

(* Approach 4 : Another OO interface *)

let rec test_flow_oo flow =
  Flow_oo.read_into flow buf >>= function
  | Error `Eof -> Lwt.return_unit
  | Error (`Msg m) -> failwith m
  | Ok n ->
    Flow_oo.write flow (Cstruct.sub buf 0 n) >>= function
    | Ok () -> test_flow_oo flow
    | Error (`Msg m) -> failwith m

(* Approach 5 : Using the read call for zero-copy *)

let rec test_flow_oo2 flow =
  Flow_oo.read flow >>= function
  | Error `Eof -> Lwt.return_unit
  | Error (`Msg m) -> failwith m
  | Ok buf ->
    Flow_oo.write flow buf >>= function
    | Ok () -> test_flow_oo2 flow
    | Error (`Msg m) -> failwith m

(* Check they work *)

let () =
  Lwt_main.run begin
    print_endline "Mirage_flow_data";
    let flow = Mirage_flow_data.create () in
    Test_mirage_flow_data.test flow >>= fun () ->
    Mirage_flow_data.close flow >>= fun () ->
    print_endline "Conduit";
    Conduit_data.create () >>= fun flow ->
    test_conduit flow >>= fun () ->
    Conduit_lwt.close flow >|= Result.get_ok >>= fun () ->
    print_endline "Conduit_oo";
    let flow = Conduit_oo.create_data () in
    test_conduit_oo flow >>= fun () ->
    Conduit_oo.close flow >|= Result.get_ok >>= fun () ->
    print_endline "Flow_oo";
    let flow = Flow_oo.create_data () in
    test_flow_oo flow >>= fun () ->
    Flow_oo.close flow >|= Result.get_ok >>= fun () ->
    print_endline "Flow_oo2";
    let flow = Flow_oo.create_data () in
    test_flow_oo2 flow >>= fun () ->
    Flow_oo.close flow >|= Result.get_ok
  end

(* Benchmark *)

let () =
  Core.Command.run (Bench.make_command [
      Bench.Test.create ~name:"mirage_flow_null" (fun () -> Test_mirage_flow_null.test ());
      Bench.Test.create ~name:"conduit_null" (fun () -> test_conduit conduit_null);
      Bench.Test.create ~name:"conduit_oo_null" (fun () -> test_conduit_oo Conduit_oo.null);
      Bench.Test.create ~name:"oo_null" (fun () -> test_flow_oo Flow_oo.null);

      Bench.Test.create ~name:"mirage_flow_data" (fun () -> Test_mirage_flow_data.test (Mirage_flow_data.create ()));
      Bench.Test.create ~name:"conduit_data" (fun () -> Conduit_data.create () >>= test_conduit);
      Bench.Test.create ~name:"conduit_oo_data" (fun () -> test_conduit_oo (Conduit_oo.create_data ()));
      Bench.Test.create ~name:"oo_data" (fun () -> test_flow_oo (Flow_oo.create_data ()));
      Bench.Test.create ~name:"oo_data2" (fun () -> test_flow_oo2 (Flow_oo.create_data ()));
    ])
