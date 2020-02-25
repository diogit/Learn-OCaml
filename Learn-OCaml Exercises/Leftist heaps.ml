let empty : heap =
  E
;;

let rank (h : heap) : rank =
  match h with
  | E -> 0
  | T(r, e, hl, hr) -> r
;;

let makeT (x : element) (h1 : heap) (h2 : heap) : heap =
  match h1, h2 with
  | E, E -> T(1, x, E, E) 
  | E, h | h, E -> T(1, x, h, E)
  | h1, (T(rr, elr, h1r, h2r) as h2) -> T(rr+1, x, h1, h2)
;;  

let singleton (x : element) : heap = T(1, x, E, E) ;;

let rec union (h1 : heap) (h2 : heap) : heap =
  match h1, h2 with
  | E, E -> E
  | E, h2 -> h2
  | h1, E -> h1
  | (T(rl, ell, h1l, h2l) as h1), (T(rr, elr, h1r, h2r) as h2) ->
      if priority ell <= priority elr
      then makeT ell h1l (union h2l h2)
      else makeT elr h1r (union h1 h2r)
;;

let insert (x : element) (h : heap) : heap =
  match h with
  | E -> singleton x
  | T(r, el, h1, h2) ->       
      if priority x <= priority el
      then makeT x h1 (union (singleton el) h2)
      else makeT el h1 (union (singleton x) h2)
;;

let extract (h : heap) : (element * heap) option =
  match h with
  | E -> None
  | T(r, x, h1, h2) -> Some(x, union h1 h2)
;;