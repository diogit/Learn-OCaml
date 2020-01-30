let last_character str = str.[String.length str - 1];;

let string_of_bool truth = 
  match truth with
  | false -> "false"
  | true -> "true";;
