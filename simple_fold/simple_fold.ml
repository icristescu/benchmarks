let ( >>= ) = Lwt.Infix.( >>= )
let ( >|= ) = Lwt.Infix.( >|= )

module StepMap = struct
  module X = struct
    type t = string

    let compare = String.compare
  end

  include Map.Make (X)
end

type map = string StepMap.t

and t = Map of map

let of_map m = Map m
let empty = of_map StepMap.empty

let add t step v =
  let (Map m) = t in
  StepMap.add step v m |> of_map

let rcons t s = t @ [ s ]

let stack_size p =
  let stats = Gc.stat () in
  Fmt.epr "stack_size at position %d is %d \n%!" p stats.Gc.stack_size

type ('a, 'r) cont = ('a -> 'r Lwt.t) -> 'r Lwt.t

type ('v, 'acc, 'r) folder =
  path:string list -> 'acc -> int -> 'v -> ('acc, 'r) cont

let fold : type acc. path:string list -> t -> acc -> acc Lwt.t =
 fun ~path t acc ->
  let counter = ref 0 in
  let rec aux : type r. (t, acc, r) folder =
   fun ~path acc d t k ->
    let next acc =
      let (Map m) = t in
      (map [@tailcall]) ~path acc d (Some m) k
    in
    next acc
  and step : type r. (string * string, acc, r) folder =
   fun ~path:_ acc _d (_s, _v) k ->
    let apply () =
      incr counter;
      if !counter mod 20_000 = 0 then stack_size !counter;
      k acc
    in
    apply ()
  and steps : type r. ((string * string) Seq.t, acc, r) folder =
   fun ~path acc d s k ->
    match s () with
    | Seq.Nil -> (k [@tailcall]) acc
    | Seq.Cons (h, t) ->
        (step [@tailcall]) ~path acc d h (fun acc ->
            (steps [@tailcall]) ~path acc d t k)
  and map : type r. (map option, acc, r) folder =
   fun ~path acc d m k ->
    match m with
    | None -> k acc
    | Some m ->
        let bindings = StepMap.to_seq m in
        (steps [@tailcall]) ~path acc d bindings k
  in
  aux ~path acc 0 t Lwt.return

let test () =
  let size = 830829 in
  let t =
    List.init size string_of_int
    |> List.fold_left (fun acc i -> add acc i i) empty
  in
  fold ~path:[] t [] >|= ignore

let () = Lwt_main.run (test ())
