let rec even l =
  match l with
  | [] -> [] 
  | x::xs -> x::odd xs
and odd l =
  match l with
  | [] -> []
  | x::xs -> even xs
;;

let rec merge l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | [], l2 -> l2
  | l1, [] -> l1
  | x::xs, y::ys -> if x <= y
      then x::merge xs l2
      else y::merge l1 ys 
;;

let rec sort l =
  let rec isSorted l =
    match l with
    | [] -> true
    | x::xs -> match xs with
      | [] -> true
      | y::ys -> x <= y && isSorted xs
  in
  let even = even l in
  let odd = odd l in
  match isSorted even, isSorted odd with
  | false, false -> merge (sort even) (sort odd)
  | false, true -> merge (sort even) odd
  | true, false -> merge even (sort odd)
  | true, true -> merge even odd
;;

let rec uniq l = 
  match l with
  | [] -> []
  | x::xs -> match xs with
    | [] -> [x]
    | y::_ -> if x = y
        then uniq xs
        else x::uniq xs
;;

let weed l =
  uniq (sort l)
;;