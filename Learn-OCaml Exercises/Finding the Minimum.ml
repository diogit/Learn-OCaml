let rec min a =
  let l = Array.to_list a in
  match l with
  | [] -> max_int
  | x::xs -> if x < (min (Array.of_list xs)) then x else (min (Array.of_list xs));;
                                    
let rec min_index a =
  let l = Array.to_list a in
  let mina = min a in
  match l with
  | [] -> -1
  | x::xs -> if x = mina then 0 else 1 + min_index (Array.of_list xs);;

let it_scales = "no" ;;

let rec minl a =
  match a with
  | [] -> max_int
  | x::xs -> if x < (minl xs) then x else (minl xs);;
                                    
let rec min_indexl a =
  let mina = minl a in
  match a with
  | [] -> -1
  | x::xs -> if x = mina then 0 else 1 + min_indexl xs;;
