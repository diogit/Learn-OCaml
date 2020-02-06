let create size = Array.make (size + 1) 0 ;;

let push buf elt = 
  let cap = buf.(0) in
  if cap = (Array.length buf) - 1
  then raise Full
  else buf.(0) <- (cap + 1);  buf.(cap + 1) <- elt;;

let rec append buf arr =
  let length = Array.length arr in
  match length with
  | 0 -> ()
  | _ ->
      push buf (arr.(length - 1));
      append buf (Array.sub arr 0 (length - 1));;

let pop buf = 
  let cap = buf.(0) in
  if cap = 0
  then raise Empty
  else buf.(0) <- buf.(0) - 1; buf.(cap);;
