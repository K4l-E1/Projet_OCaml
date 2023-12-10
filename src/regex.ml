open Regex_base

let rec repeat n l =
  if n <= 0 then [] 
  else l @ repeat (n-1) l

let rec expr_repeat n e =
  if n <= 0 then Eps
  else Concat (e, expr_repeat (n-1) e)

let rec is_empty e =
  failwith "À compléter"

let rec null e =
  match e with 
  | Eps -> true
  | Base a -> false
  | Joker -> false
  | Concat (a,b) -> null a && null b
  | Alt (a,b) -> null a || null b
  | Star a -> true

let rec is_finite e =
  failwith "À compléter"

let product l1 l2 =
  let rec aux l3 l4 =
    match l3 with
    | [] -> []
    | a :: b -> List.map (fun el -> a @ el) l4 @ aux b l4
  in aux l1 l2

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  let rec aux l =
    match e with 
    | Eps -> []
    | Base a -> [a]
    | Joker -> []
    | Concat (a,b) -> alphabet_expr a @ alphabet_expr b
    | Alt (a,b) -> alphabet_expr a @ alphabet_expr b
    | Star a -> alphabet_expr a
  in sort_uniq (aux e)

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
