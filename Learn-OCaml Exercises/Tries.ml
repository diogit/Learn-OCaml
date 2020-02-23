let rec print_path path =
  match path with
  | [] -> ()
  | x::[] -> print_string x
  | x::xs -> print_string x; print_string "/"; print_path xs;;

let rec print_file lvl name =
  if lvl = 0
  then print_string name
  else (print_string "| "; print_file (lvl - 1) name);;

let rec print_symlink lvl name path =
  if lvl = 0
  then (print_string name; print_string " -> "; print_path path)
  else (print_string "| "; print_symlink (lvl - 1) name path);;

let rec print_dir lvl name =
  if lvl = 0
  then (print_string "/"; print_string name)
  else (print_string "| "; print_dir (lvl - 1) name);;

let print_filesystem root =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec print_filesystem lvl items =
    match items with
    | [] -> ()
    | x::xs -> (
        match x with 
        | name, File -> print_file lvl name; print_newline ();
        | name, Dir items -> print_dir lvl name;
            print_newline ();
            print_filesystem (lvl + 1) items
        | name, Symlink path -> print_symlink lvl name path;
            print_newline ();
      ); print_filesystem lvl xs
  in
  print_filesystem 0 root ;;

let rec resolve sym path =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec resolve acc path =
    "Replace this string with your implementation."  in
  resolve (List.tl (List.rev sym)) path ;;

let rec file_exists root path =
  match root, path with 
  | (name, File)::rs, pname::_ -> String.equal name pname ||
                                  file_exists rs path
  | (rname, Dir dir)::rs, pname::ps -> (String.equal rname pname
                                        && file_exists dir ps)
                                       || file_exists rs path
  | (_, Symlink _)::rs, _ -> file_exists rs path
  | _, [] | [], _ -> false
                               

(* move print_filesystem here for exercise 8 *)
