let exchange k = ((k mod 10) * 10) + (k / 10);;

let is_valid_answer (grand_father_age, grand_son_age) =
  let (ex_grand_father_age, ex_grand_son_age)
    = exchange grand_father_age, exchange grand_son_age in
  grand_father_age = grand_son_age * 4
  && ex_grand_son_age = ex_grand_father_age * 3;;

let find answer = 
  let maxfather = fst answer and minson = snd answer in
  let rec computeAnswer son = 
    let father = son * 4 in
    if father > maxfather then (-1, -1) else
    if is_valid_answer (father, son)
    then (father, son)
    else computeAnswer (son + 1)
  in computeAnswer minson
;;