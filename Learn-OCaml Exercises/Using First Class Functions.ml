let rec compose = function l ->
  function v ->
    match l with
    | [] -> v
    | x::xs -> x (compose xs v);;

let rec fixedpoint f start delta =
  let y = f start in
  if abs_float (y -. f y) < delta then y else fixedpoint f y delta;;
