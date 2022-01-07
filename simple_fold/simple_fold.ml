module StepMap = struct
  module X = struct
    type t = string

    let compare = String.compare
  end

  include Map.Make (X)
end

type t = string StepMap.t

let add t step v = StepMap.add step v t
let rcons t s = t @ [ s ]

let stack_size p =
  let stats = Gc.stat () in
  Fmt.epr "stack_size at position %d is %d \n%!" p stats.Gc.stack_size

type ('a, 'r) cont = ('a -> 'r) -> 'r

type ('v, 'acc, 'r) folder =
  path:string list -> 'acc -> int -> 'v -> ('acc, 'r) cont

(* If we remove the path or the depth argument the test passes. *)
let fold : type acc. path:string list -> t -> acc -> acc =
 fun ~path t acc ->
  let counter = ref 0 in
  let rec step : type r. (string * string, acc, r) folder =
   fun ~path:_ acc _d _h k ->
    incr counter;
    if !counter mod 20_000 = 0 then stack_size !counter;
    k acc
  and steps : type r. ((string * string) Seq.t, acc, r) folder =
   fun ~path acc d s k ->
    match s () with
    | Seq.Nil -> (k [@tailcall]) acc
    | Seq.Cons (h, t) ->
        let steps' acc = (steps [@tailcall]) ~path acc d t k in
        (step [@tailcall]) ~path acc d h steps'
  and map : type r. (t, acc, r) folder =
   fun ~path acc d t k ->
    let bindings = StepMap.to_seq t in
    (steps [@tailcall]) ~path acc d bindings k
  in
  map ~path acc 0 t Fun.id

let test () =
  let size = 830829 in
  let t =
    List.init size string_of_int
    |> List.fold_left (fun acc i -> add acc i i) StepMap.empty
  in
  fold ~path:[] t [] |> ignore

let () = test ()
