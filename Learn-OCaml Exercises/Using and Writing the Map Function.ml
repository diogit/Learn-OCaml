let wrap l = 
  List.map (fun x -> [x]) l;;

let rec tree_map f = function t ->
  match t with 
  | Leaf(v) -> Leaf(f v)
  | Node(l, v, r) -> Node(tree_map f l, f v, tree_map f r);;
