let display_image width height f_image =
  for i = 0 to width do
    for j = 0 to height do
      print_string (if f_image j i then "#" else " ")
    done;
    print_newline ()
  done;;

let rec render blend x y =
  match blend with
  | Image f -> f x y
  | And(f, g) -> render f x y && render g x y
  | Or(f, g) -> render f x y || render g x y
  | Rem(f, g) -> render f x y && not (render g x y);;

let display_blend width height blend = 
  for i = 0 to width do
    for j = 0 to height do
      print_string (if render blend j i then "#" else " ")
    done;
    print_newline ()
  done;;