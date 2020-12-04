We have several different flow APIs in Mirage (e.g. mirage-flow and Conduit 3). This repository benchmarks 4 possible APIs:

1. Mirage-flow (functors)
2. Conduit 3 (GADTs, 1st-class modules)
3. Conduit OO, an API in the style of Conduit 3 using objects
4. Flow OO, another OO API which I just made up

# Generic copy function

For each of these, I wrote a function that simply copies its input to its output.
This function is generic and can work with any concrete implementation.
They are:

## Mirage-flow copy

```ocaml
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
```

Note: For mirage-flow, we need to apply the functor whenever we use this, which the other schemes avoid.
This is the fastest option, since there's no dynamic dispatch.
It's mostly here to establish a base-line for performance.

## Conduit 3

```ocaml
let rec test_conduit flow =
  Conduit_lwt.recv flow buf >>= function
  | Error (`Msg m) -> failwith m
  | Error `Not_found -> assert false
  | Ok `End_of_flow -> Lwt.return_unit
  | Ok (`Input n) ->
    let rec aux i =
      if i = n then test_conduit flow
      else (
        Conduit_lwt.send flow (Cstruct.sub buf i (n - i)) >>= function
        | Ok j -> aux (i + j)
        | Error `Not_found -> assert false
        | Error (`Msg m) -> failwith m
      )
    in
    aux 0
```

Conduit 3 requires us to consider the possibilty of partial writes, which is annoying.
It also makes us handle the `Not_found` error, which doesn't make much sense for reads and writes.

## Conduit-style OO

```ocaml
let rec test_conduit_oo flow =
  Conduit_oo.recv flow buf >>= function
  | Error (`Msg m) -> failwith m
  | Ok `End_of_flow -> Lwt.return_unit
  | Ok (`Input n) ->
    let rec aux i =
      if i = n then test_conduit_oo flow
      else (
        Conduit_oo.send flow (Cstruct.sub buf i (n - i)) >>= function
        | Ok j -> aux (i + j)
        | Error (`Msg m) -> failwith m
      )
    in
    aux 0
```

This looks much like using Conduit to the user. I did remove the `Not_found` errors though.

## Flow OO

```ocaml
let rec test_flow_oo flow =
  Flow_oo.read_into flow buf >>= function
  | Error `Eof -> Lwt.return_unit
  | Error (`Msg m) -> failwith m
  | Ok n ->
    Flow_oo.write flow (Cstruct.sub buf 0 n) >>= function
    | Ok () -> test_flow_oo flow
    | Error (`Msg m) -> failwith m
```

This is my preferred API. Like Conduit, we read into a buffer.
However, it also provides a mirage-flow-style `read` method.
It uses inheritance to make this default to reading into a fresh 4096 byte buffer
if the implementation doesn't provide a more efficient version.

# Null device

For each API, I implemented a `/dev/null` style device. Reads always return end-of-file, and writes discard the data.
Here are the implementations:

## Mirage-flow

```ocaml
type error = |
type write_error = [ `Closed ]

let pp_error _f = function (_:error) -> .
let pp_write_error = Mirage_flow.pp_write_error

type flow = unit

let read () = Lwt_result.return `Eof
let write () _buf = Lwt_result.return ()
let writev () _bufs = Lwt_result.return ()
let close () = Fmt.invalid_arg
```

Pretty straight-forward.

## Conduit 3

```ocaml
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
```

Some registration stuff that I don't quite understand.
Also, we need to provide a connect function with a fixed signature here.

## Conduit-style OO

```ocaml
let null = object (_ : flow)
  method recv _buf = Lwt_result.return `End_of_flow
  method send buf = Lwt_result.return (Cstruct.len buf)
  method close = Fmt.invalid_arg "close null!"
  method cast _ = None
end
```

The `cast` method isn't used in these tests, but Conduit 3 allows checking for extra features at runtime,
so I added a similar feature here.

## Flow OO

```ocaml
let null = object (_ : flow)
  inherit flow
  method read_into _buf = Lwt_result.fail `Eof
  method write _buf = Lwt_result.return ()
  method close = Fmt.invalid_arg "close null!"
end
```

This uses some inheritance to get a free `read` method and a default `cast` that doesn't allow casting to anything.
Note: I also made `Eof` an error here.
It's not exactly an error, but it does avoid an extra allocation in the common case where you return `Ok data` rather
than ``Ok (`Data data)``.

# Benchmarks

I ran the copy operations first using the null-flows defined above, and then again using some flows with real data.
Here are the results on my machine for the null flows:

```
┌──────────────────┬──────────┬─────────┬────────────┐
│ Name             │ Time/Run │ mWd/Run │ Percentage │
├──────────────────┼──────────┼─────────┼────────────┤
│ mirage_flow_null │   9.29ns │  25.00w │     29.11% │
│ conduit_null     │  31.92ns │  55.00w │    100.00% │
│ conduit_oo_null  │  12.29ns │  29.00w │     38.51% │
│ oo_null          │  11.49ns │  24.00w │     35.99% │
└──────────────────┴──────────┴─────────┴────────────┘
```

In summary, Conduit 3 is the slowest by some margin.
The OO APIs give you dynamic dispatch with very little overhead, even in the trivial null case.
For real flows we'd expect dealing with the data to take more time and the overhead to matter less.

Reading from the data flows produces a 4096 test message in 10 byte chunks.
Writing to it just records the data.
Closing it checks that the received data matches the test message.

The results with some test data are:

```
┌──────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name             │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ mirage_flow_data │  12.63us │ 24.23kw │    2.62w │    2.62w │     34.34% │
│ conduit_data     │  36.77us │ 52.43kw │    7.33w │    7.33w │    100.00% │
│ conduit_oo_data  │  20.65us │ 31.06kw │    6.54w │    6.54w │     56.16% │
│ oo_data          │  19.87us │ 23.26kw │    6.37w │    6.37w │     54.05% │
│ oo_data2         │  15.97us │ 23.26kw │    6.46w │    6.46w │     43.45% │
└──────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
```

I made two versions of the flow OO test, using both `read_into` and `read`.
For this particular case, `read` is faster because the test data is a fixed
string and we can just return a view onto that in `read`, avoiding a copy.
`mirage_flow_data` and `oo_data2` both take advantage of this, although perhaps
it's not very realistic.

# Flow API

The public API for the OO flow system is in `flow_oo.mli`.
It has a couple of other interesting features.
First, the read functions (for example) are only defined to require an object
with the read methods, so you could also make one-way streams and reuse parts of
the API. Functions consuming flows can indicate whether they will read, write
or do both.

It is also possible to check for extra features both statically and dynamically.
For example, if you need be able to change keys, you could define an extension of `flow`
like this:

```ocaml
type key_flow = < flow; new_key : unit >
```

Then a function requiring key changes can take this extended type.
The compiler can check at compile-time that it is passed a flow with the `new_key` feature.

Alternatively, a flow can support runtime checks for extra features.
For example:

```ocaml
type key_flow = < flow; new_key : unit >
type _ ty += Key_flow : key_flow ty
```

Then you can check whether a `flow` can be upgraded to the `key_flow` interface:

```ocaml
  match cast flow Key_flow with
  | Some flow -> new_key flow
  | None -> ()	(* Not supported *)
```
