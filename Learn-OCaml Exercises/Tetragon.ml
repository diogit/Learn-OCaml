let diff p1 p2 = fst p1 <> fst p2 || snd p1 <> snd p2;;

let pairwise_distinct (lup, rup, llp, rlp) =
  diff lup rup && diff lup llp && diff lup rlp
  && diff rup llp && diff rup rlp
  && diff llp rlp;;

let wellformed (lup, rup, llp, rlp) =
  fst llp < fst rlp
  && fst llp < fst rup
  && fst lup < fst rup
  && fst lup < fst rlp
  && snd llp < snd lup
  && snd rlp < snd rup;;

let rotate_point (x, y) = (y, -x) ;;

let reorder (p1, p2, p3, p4) =
  if wellformed (p1, p2, p3, p4) then (p1, p2, p3, p4)
  else if wellformed (p1, p2, p4, p3) then (p1, p2, p4, p3) 
  else if wellformed (p1, p3, p2, p4) then (p1, p3, p2, p4)
  else if wellformed (p1, p3, p4, p2) then (p1, p3, p4, p2)
  else if wellformed (p1, p4, p2, p3) then (p1, p4, p2, p3)
  else if wellformed (p1, p4, p3, p2) then (p1, p4, p3, p2)
  else if wellformed (p2, p1, p3, p4) then (p2, p1, p3, p4)
  else if wellformed (p2, p1, p4, p3) then (p2, p1, p4, p3)
  else if wellformed (p2, p3, p1, p4) then (p2, p3, p1, p4)
  else if wellformed (p2, p3, p4, p1) then (p2, p3, p4, p1)
  else if wellformed (p2, p4, p1, p3) then (p2, p4, p1, p3)
  else if wellformed (p2, p4, p3, p1) then (p2, p4, p3, p1)
  else if wellformed (p3, p1, p2, p4) then (p3, p1, p2, p4)
  else if wellformed (p3, p1, p4, p2) then (p3, p1, p4, p2)
  else if wellformed (p3, p2, p1, p4) then (p3, p2, p1, p4)
  else if wellformed (p3, p2, p4, p1) then (p3, p2, p4, p1)
  else if wellformed (p3, p4, p1, p2) then (p3, p4, p1, p2)
  else if wellformed (p3, p4, p2, p1) then (p3, p4, p2, p1)
  else if wellformed (p4, p1, p2, p3) then (p4, p1, p2, p3)
  else if wellformed (p4, p1, p3, p2) then (p4, p1, p3, p2)
  else if wellformed (p4, p2, p1, p3) then (p4, p2, p1, p3)
  else if wellformed (p4, p2, p3, p1) then (p4, p2, p3, p1)
  else if wellformed (p4, p3, p1, p2) then (p4, p3, p1, p2)
  else  (p4, p3, p2, p1)
;;

let rotate_tetragon (lup, rup, llp, rlp) = 
  (rotate_point llp,
   rotate_point lup,
   rotate_point rlp,
   rotate_point rup);;
