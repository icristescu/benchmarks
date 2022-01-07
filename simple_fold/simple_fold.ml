let ( >>= ) = Lwt.Infix.( >>= )
let ( >|= ) = Lwt.Infix.( >|= )

type step = string

module StepMap = struct
  module X = struct
    type t = step

    let compare = String.compare
  end

  include Map.Make (X)
end

type elt = [ `Node of t | `Contents of string ]

and map = elt StepMap.t

and info = { mutable map : map option }

and v = Map of map

and t = { mutable v : v; info : info }

let of_map m =
  let info = { map = Some m } in
  let v = Map m in
  { v; info }

let cached_map t =
  match (t.v, t.info.map) with
  | Map m, None ->
      let m = Some m in
      t.info.map <- m;
      m
  | _, m -> m

let empty = of_map StepMap.empty

let add t step v : t =
  let (Map m) = t.v in
  StepMap.add step v m |> of_map

let rcons t s = t @ [ s ]

let stack_size p =
  let stats = Gc.stat () in
  Fmt.epr "stack_size at position %d is %d \n%!" p stats.Gc.stack_size

type ('a, 'r) cont = ('a -> 'r) -> 'r
type ('a, 'r) cont_lwt = ('a, 'r Lwt.t) cont

type ('v, 'acc, 'r) folder =
  path:step list -> 'acc -> int -> 'v -> ('acc, 'r) cont_lwt

type 'a node_fn = step list -> step list -> 'a -> 'a Lwt.t

let fold : type acc. path:step list -> t -> acc -> acc Lwt.t =
 fun ~path t acc ->
  let counter = ref 0 in
  let rec aux : type r. (t, acc, r) folder =
   fun ~path acc d t k ->
    let next acc =
      match cached_map t with
      | Some n -> (map [@tailcall]) ~path acc d (Some n) k
      | None -> k acc
    in
    next acc
  and aux_uniq : type r. (t, acc, r) folder =
   fun ~path acc d t k -> (aux [@tailcall]) ~path acc d t k
  and step : type r. (step * elt, acc, r) folder =
   fun ~path acc d (s, v) k ->
    let path = rcons path s in
    match v with
    | `Node n -> (aux_uniq [@tailcall]) ~path acc (d + 1) n k
    | `Contents _c ->
        let apply () =
          incr counter;
          if !counter mod 20_000 = 0 then stack_size !counter;
          (* checkpoint "checkpoint after 50_000 contents"; *)
          k acc
        in
        apply ()
  and steps : type r. ((step * elt) Seq.t, acc, r) folder =
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
        seq ~path acc d bindings k
  and seq : type r. ((step * elt) Seq.t, acc, r) folder =
   fun ~path acc d bindings k -> (steps [@tailcall]) ~path acc d bindings k
  in
  aux_uniq ~path acc 0 t Lwt.return

let id _ _ acc = Lwt.return acc

let test () =
  let size = 830829 in
  let t =
    List.init size string_of_int
    |> List.fold_left (fun acc i -> add acc i (`Contents i)) empty
  in
  fold ~path:[] t [] >|= ignore

let () = Lwt_main.run (test ())
