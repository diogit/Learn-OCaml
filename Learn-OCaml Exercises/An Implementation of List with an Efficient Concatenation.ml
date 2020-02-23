let rec to_list l =
  match l with
  | CEmpty -> []
  | CSingle a -> [a]
  | CApp(l, r) -> to_list l @ to_list r;;

let rec of_list l = 
  match l with
  | [] -> CEmpty
  | x::xs -> CApp(CSingle x, of_list xs);;

let append l1 l2 =
  match l1, l2 with
  | CEmpty, l | l, CEmpty -> l
  | l1, l2 -> CApp(l1, l2);;

let hd l =
  match to_list l with
  | [] -> None
  | hd::_ -> Some hd
;;

let tl l =
  match to_list l with
  | [] -> None
  | _::tl -> Some (of_list tl)
;;