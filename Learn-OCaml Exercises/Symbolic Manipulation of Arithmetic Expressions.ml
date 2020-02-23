let my_example = EAdd(EMul(EInt 2, EInt 2), EMul(EInt 3, EInt 3)) ;;

let rec eval e =
  match e with
  | EInt e -> e
  | EAdd(e1, e2) -> eval e1 + eval e2
  | EMul(e1, e2) -> eval e1 * eval e2;;

let rec factorize e =
  match e with
  | EAdd(EMul(e1, e2), EMul(e3, e4)) when e1 = e3 ->
      EMul(e1, EAdd(e2, e4))
  | EInt e -> EInt e
  | EAdd(e1, e2) -> EAdd(factorize e1, factorize e2)
  | EMul(e1, e2) -> EMul(factorize e1, factorize e2)

let rec expand e =
  match e with
  | EMul(a, EAdd(b, c)) | EMul(EAdd(b, c), a) -> EAdd(EMul(a, b), EMul(a, c))
  | e -> e
           (*
             | EInt e -> EInt e
             | EAdd(e1, e2) -> EAdd(expand e1, expand e2)
             | EMul(e1, e2) -> EMul(expand e1, expand e2);;*)

let rec simplify e =
  match e with
  | EMul(_, EInt 0) | EMul(EInt 0, _) -> EInt 0
  | EMul(e, EInt 1) | EMul(EInt 1, e) ->  e
  | EAdd(e, EInt 0) | EAdd(EInt 0, e) ->  e
  | e ->  e 
