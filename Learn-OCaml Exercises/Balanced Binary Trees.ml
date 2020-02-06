let rec height t = 
  match t with
  | Empty -> 0
  | Node(l, v, r) -> 1 + max (height l) (height r);;

let rec balanced t  =
  match t with
  | Empty -> true
  | Node(l, v, r) -> (height l = height r) && balanced l && balanced r;;