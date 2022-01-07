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

type elt = step

and map = elt StepMap.t

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

type ('a, 'r) cont = ('a -> 'r) -> 'r

type ('a, 'r) cont_lwt = ('a, 'r Lwt.t) cont

type ('v, 'acc, 'r) folder = path:step list -> 'acc -> 'v -> ('acc, 'r) cont_lwt

type 'a node_fn = step list -> step list -> 'a -> 'a Lwt.t

let fold :
    type acc.
    order:[ `Sorted | `Undefined | `Random ] ->
    force:bool ->
    cache:bool ->
    uniq:bool ->
    pre:acc node_fn option ->
    post:acc node_fn option ->
    path:step list ->
    ?depth:int ->
    node:(step list -> _ -> acc -> acc Lwt.t) ->
    contents:(step list -> string -> acc -> acc Lwt.t) ->
    tree:(step list -> _ -> acc -> acc Lwt.t) ->
    t ->
    acc ->
    acc Lwt.t =
 fun ~order:_ ~force:_ ~cache:_ ~uniq:_ ~pre:_ ~post:_ ~path ?depth:_ ~node:_
     ~contents:_ ~tree:_ t acc ->
  let counter = ref 0 in
  let rec aux : type r. (t, acc, r) folder =
   fun ~path acc t k ->
    let next acc =
      let (Map m) = t in
      (map [@tailcall]) ~path acc (Some m) k
    in
    next acc
  and aux_uniq : type r. (t, acc, r) folder =
   fun ~path acc t k -> (aux [@tailcall]) ~path acc t k
  and step : type r. (step * elt, acc, r) folder =
   fun ~path acc (s, _v) k ->
    let _path = rcons path s in
    let apply () =
      incr counter;
      if !counter mod 20_000 = 0 then stack_size !counter;
      k acc
    in
    apply ()
  and steps : type r. ((step * elt) Seq.t, acc, r) folder =
   fun ~path acc s k ->
    match s () with
    | Seq.Nil -> (k [@tailcall]) acc
    | Seq.Cons (h, t) ->
        (step [@tailcall]) ~path acc h (fun acc ->
            (steps [@tailcall]) ~path acc t k)
  and map : type r. (map option, acc, r) folder =
   fun ~path acc m k ->
    match m with
    | None -> k acc
    | Some m ->
        let bindings = StepMap.to_seq m in
        seq ~path acc bindings k
  and seq : type r. ((step * elt) Seq.t, acc, r) folder =
   fun ~path acc bindings k -> (steps [@tailcall]) ~path acc bindings k
  in
  aux_uniq ~path acc t Lwt.return

let id _ _ acc = Lwt.return acc

let fold ?(order = `Sorted) ?(force = true) ?(cache = false) ?(uniq = false)
    ?pre ?post ?depth ?(contents = id) ?(node = id) ?(tree = id) t acc =
  fold ~order ~force ~cache ~uniq ~pre ~post ~path:[] ?depth ~contents ~node
    ~tree t acc

let test () =
  let size = 830829 in
  let t =
    List.init size string_of_int
    |> List.fold_left (fun acc i -> add acc i i) empty
  in
  fold t [] >|= ignore

let () = Lwt_main.run (test ())
