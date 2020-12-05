We have several different flow APIs in Mirage (e.g. mirage-flow and Conduit 3). This repository benchmarks 4 possible APIs:

1. Mirage-flow (functors)
2. Conduit 3 (GADTs, 1st-class modules)
3. Conduit OO, an API in the style of Conduit 3 using objects
4. Flow OO, another OO API which I just made up

# Contents

<!-- vim-markdown-toc GFM -->

* [Generic copy function](#generic-copy-function)
	* [Mirage-flow copy](#mirage-flow-copy)
	* [Conduit 3](#conduit-3)
	* [Conduit-style OO](#conduit-style-oo)
	* [Flow OO](#flow-oo)
* [Null device](#null-device)
	* [Mirage-flow](#mirage-flow)
	* [Conduit 3](#conduit-3-1)
	* [Conduit-style OO](#conduit-style-oo-1)
	* [Flow OO](#flow-oo-1)
* [Benchmarks](#benchmarks)
* [Flow API](#flow-api)
	* [Restricting the operations](#restricting-the-operations)
	* [Extending the operations](#extending-the-operations)
	* [Custom errors](#custom-errors)
	* [Custom close APIs](#custom-close-apis)

<!-- vim-markdown-toc -->

# Generic copy function

For each of these, I wrote a function that simply copies its input to its output.
This function is generic and can work with any concrete implementation.
They are:

## Mirage-flow copy

```ocaml
module Test(F : Mirage_flow.S) = struct
  let rec copy flow =
    F.read flow >>= function
    | Ok `Eof -> Lwt_result.return ()
    | Error e -> Lwt_result.fail (`Read e)
    | Ok (`Data buf) ->
      F.write flow buf >>= function
      | Error e -> Lwt_result.fail (`Write e)
      | Ok () -> copy flow
end
```

Notes:
- For mirage-flow, we need to apply the functor whenever we use this, which the other schemes avoid.
- The error types are incompatible, so we have to handle them separately.

This is the fastest option, since there's no dynamic dispatch.
It's mostly here to establish a base-line for performance.

## Conduit 3

```ocaml
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
```

Notes:
- Conduit 3 requires us to consider the possibilty of partial writes, which is annoying.
- Conduit 3 compresses all the errors into just `Msg` or `Not_found`,
  so you can't match on protocol-specific errors.

## Conduit-style OO

```ocaml
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
```

Although using objects internally, to the user of a flow it looks identical to Conduit.

## Flow OO

```ocaml
let rec copy_flow_oo flow =
  Flow_oo.read_into flow buf >>!= function
  | `Eof -> Lwt_result.return ()
  | `Input n ->
    Flow_oo.write flow (Cstruct.sub buf 0 n) >>!= fun () ->
    copy_flow_oo flow
```

This is my preferred API. Like Conduit, we read into a buffer.
However, it also provides a mirage-flow-style `read` method.
It uses inheritance to make this default to reading into a fresh 4096 byte buffer
if the implementation doesn't provide a more efficient version.

Here's an alternative version using that:

```ocaml
let rec copy_flow_oo2 flow =
  Flow_oo.read flow >>!= function
  | `Eof -> Lwt_result.return ()
  | `Data buf ->
    Flow_oo.write flow buf >>!= fun () ->
    copy_flow_oo2 flow
```

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
let close () = Fmt.invalid_arg "close null!"
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
  method! read = Lwt_result.return `Eof
  method read_into _buf = Lwt_result.return `Eof
  method write _buf = Lwt_result.return ()
end
```

This uses some inheritance to get a default `cast` that doesn't allow casting to anything.
We could have written it explicitly as above, but using `inherit` allows adding more methods later without breaking existing code.
We would also have inherited the default `read`, but for the null device we don't need a buffer so we can save allocating it.

Notes:
- I've never found a use for resuming from partial writes, so I used the mirage-flow approach of returning `()`
  from writes. You can return an error value containing the partial write information if desired.
- I removed the `close` method from the `flow` type, eliminating the fake close operation needed with the other APIs.
  See the notes at the end for details.

# Benchmarks

I ran the copy operations first using the null-flows defined above, and then again using some flows with real data.
Here are the results on my machine for the null flows:

```
┌──────────────────┬──────────┬─────────┬────────────┐
│ Name             │ Time/Run │ mWd/Run │ Percentage │
├──────────────────┼──────────┼─────────┼────────────┤
│ mirage_flow_null │  20.33ns │  52.00w │     48.97% │
│ conduit_null     │  41.51ns │  82.00w │    100.00% │
│ conduit_oo_null  │  21.99ns │  56.00w │     52.98% │
│ oo_null          │  20.99ns │  51.00w │     50.56% │
│ oo_null2         │  20.66ns │  51.00w │     49.77% │
└──────────────────┴──────────┴─────────┴────────────┘
```

In summary, Conduit 3 is the slowest by some margin.
The OO APIs give you dynamic dispatch with very little overhead, even in the trivial null case.
For real flows we'd expect dealing with the data to take more time and the overhead to matter less.

Note: I originally had the copy code throwing exceptions.
      I've switched to using `Lwt_result` as that's probably more realistic.
      Using `Lwt_result` adds some overhead to all the times,
      which helps Conduit a bit, but the pattern is the same.

Reading from the data flows produces a 4096 byte test message in 10 byte chunks.
Writing to it just records the data.
Closing it checks that the received data matches the test message.
The results with test data are:

```
┌──────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name             │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ mirage_flow_data │  12.57us │ 23.85kw │    2.88w │    2.88w │     34.09% │
│ conduit_data     │  36.88us │ 55.74kw │    7.87w │    7.87w │    100.00% │
│ conduit_oo_data  │  21.42us │ 34.36kw │    7.41w │    7.41w │     58.09% │
│ oo_data          │  21.49us │ 27.80kw │    7.68w │    7.68w │     58.29% │
│ oo_data2         │  17.21us │ 27.80kw │    7.99w │    7.99w │     46.66% │
└──────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
```

I made two versions of the flow OO test, using both `read_into` and `read`.
For this particular case, `read` is faster because the test data is a fixed
string and we can just return a view onto that in `read`, avoiding a copy.
`mirage_flow_data` and `oo_data2` both take advantage of this, although perhaps
it's not very realistic.

# Flow API

The public API for the OO flow system is in `flow_oo.mli`.
It has a few other interesting features...

## Restricting the operations

The read functions (for example) are only defined to require an object
with the read methods, e.g.

```ocaml
val read_into : #reader -> Cstruct.t -> ([`Input of int | `Eof], [> `Flow of error]) Lwt_result.t
val read : #reader -> ([`Data of Cstruct.t | `Eof], [> `Flow of error]) Lwt_result.t
```

This means you could also make one-way streams and reuse parts of
the API. Functions consuming flows can indicate whether they will read, write
or do both by taking an argument of type `#reader`, `#writer` or `#flow`.

## Extending the operations

It is possible to check for extra features both statically and dynamically.
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

## Custom errors

The flow API uses an open type for errors:

```ocaml
type flow_error = ..
type error = flow_error * (Format.formatter -> unit)
val pp_error : error Fmt.t
```

This means that you can add your own concrete error types and then match on them if desired,
or fall back to using `pp_error` to just print them.

## Custom close APIs

As an experiment, the flow API does not include a close method by default.
Instead, each flow's constructor will include some means of closing it, which
can depend on the flow. For example:

- The constructor can take a `Lwt_switch` to limit the flow's lifetime.
- The constructor could return a `close` function along with the flow.
- The flow can be extended to have a `close` method.
- A singleton flow (such as `null`) might not provide any way to close itself.

This also makes the lifecycle of a flow more obvious: a generic flow function is not expected to close the flow
itself (its caller should handle that). It also allows different closing APIs (for example, synchronous or not,
able to return errors or not).
