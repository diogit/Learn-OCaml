let rec lookup_function n = 
  function env ->
    match env with
    |  [] -> invalid_arg "lookup_function"
    | (name, op)::xs -> if String.equal name n
        then op
        else (lookup_function n) xs ;;

let add_function name op env = (name, op)::env;;

let my_env = ("min", fun x y -> if x - y < 0 then x else y)::initial_env;;

let rec compute env op =
  match op with
  | Value(n) -> n
  | Op(name, a, b) -> let f = lookup_function name env in
      f (compute env a) (compute env b);; 

let rec compute_eff env =
  function op ->
    match op with
    | Value(n) -> n
    | Op(name, a, b) ->
        (lookup_function name env) (compute_eff env a) (compute_eff env b);; 
