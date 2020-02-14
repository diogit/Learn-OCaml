let rec print_int_list l =
  match l with
  | [] -> ()
  | x::xs -> print_int x; print_newline (); print_int_list xs;;

let print_every_other k l =
  ignore
    (List.fold_left (fun acc el ->
         if (acc mod k) = 0
         then (print_int el; print_newline (); 1)
         else acc + 1) 0 l);;

let rec print_list print l =
  match l with
  | [] -> ()
  | x::xs -> print x; print_newline (); print_list print xs;;
