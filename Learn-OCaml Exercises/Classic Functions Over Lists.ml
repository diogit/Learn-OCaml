let rec mem x l = 
  match l with
  | [] -> false
  | hd::tl -> x = hd || mem x tl;;

let rec append l1 l2 =
  match l1 with
  | [] -> l2 
  | x::xs -> x :: append xs l2 ;;

let rec combine l1 l2 =
  match l1, l2 with 
  | [], _ -> []
  | _, [] -> []
  | x::xs, y::ys -> (x, y) :: combine xs ys;;

let rec assoc l k = 
  match l with
  | [] -> None
  | (s, n)::xs -> if String.compare s k = 0 then Some n else assoc xs k;;
