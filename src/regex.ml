open Regex_base

let rec repeat n l =
  if n <= 0 then [] 
  else l @ repeat (n-1) l

let rec expr_repeat n e =
  if n <= 0 then Eps
  else Concat (e, expr_repeat (n-1) e)

let rec is_empty e =
        match e with
        | Base a -> false
        | Eps -> true
        | Joker -> false
        | Concat (a,b) -> is_empty a && is_empty b
        | Alt (a,b) -> is_empty a && is_empty b
        | Star a -> is_empty a


let rec null e =
  match e with 
  | Eps -> true
  | Base a -> false
  | Joker -> false
  | Concat (a,b) -> null a && null b
  | Alt (a,b) -> null a || null b
  | Star a -> true

let rec is_finite e =
  match e with
        | Base a -> true
        | Eps -> true
        | Joker -> true
        | Concat (a,b) -> is_finite a && is_finite b
        | Alt (a,b) -> is_finite a && is_finite b
        | Star a -> 
                        match a with
                        | Eps -> true
                        | Concat(a,b) -> a = Eps && b = Eps
                        | Alt(a,b) -> a = Eps && b = Eps
                        | Joker -> false
                        | Base a -> false
                        | Star a -> is_finite a

        


let product l1 l2 =
  let rec aux l3 l4 =
    match l3 with
    | [] -> []
    | a :: b -> List.map (fun el -> a @ el) l4 @ aux b l4
  in aux l1 l2

let rec enumerate alphabet e =
        if is_finite e then
        match e with
        | Eps -> Some [[]]
        | Base c -> Some [[c]]
        | Joker -> Some (List.map (fun c -> [c]) alphabet)
        | Concat (e1, e2) ->
                        (match (enumerate alphabet e1, enumerate alphabet e2) with
                                | (Some l1, Some l2) -> Some (product l1 l2)
                                | _ -> None)
        | Alt (e1, e2) -> 
                        (match (enumerate alphabet e1, enumerate alphabet e2) with
                        | (Some l1, Some l2) -> Some (List.sort_uniq compare (l1 @ l2))
                        | _ -> None)

        | Star e -> (match enumerate alphabet e with
                | Some l -> Some (List.flatten (List.init 10 (fun n -> product l l)))       
                | None -> None)
        else None

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

let rec accept_partial e w =
  let alphabet_e = alphabet_expr e in
  let alphabet = union_sorted alphabet_e w in
  match enumerate alphabet e with
  | None -> Infinite
  | Some res -> if List.mem w res then Accept else Reject
  