let is_multiple i x = i mod x = 0;;

let output_multiples x n m = 
  for i = n to m do
    if is_multiple i x then (print_int i; print_string ",") else ()
  done
;;

exception Zero;;
let display_sign_until_zero f m =
  let display f m =
    for i = 0 to m do
      if f i = 0 then raise Zero else ();
      print_string (if f i > 0 then "positive" else "negative");
      print_newline ();
    done;
  in try display f m with
    Zero -> print_string "zero"
;;
