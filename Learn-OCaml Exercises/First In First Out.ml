let is_empty (front, back) =
  match front, back with
  | [], [] -> true
  | _, _ -> false;;

let enqueue x (front, back) =
  (front, x::back);;

let split l = 
  let half = List.length l / 2 in
  let rec insert l acc h = 
    match l with
    | [] -> [], List.rev acc
    | x::xs -> if h = 0
        then List.rev (x::xs), List.rev acc
        else insert xs (x::acc) (h - 1)
  in insert l [] half;;


let dequeue (front, back) =
  match front with
  | x::xs -> (x, (xs, back))
  | [] -> match List.rev back with
    | [] -> (-1, (front, back)) 
    | y::ys -> (y, (ys, []));;
