let rec is_sorted a = 
  let l = Array.to_list a in
  match l with
  | [] -> true
  | x::xs -> match xs with
    | [] -> true
    | y::ys -> String.compare x y = -1 && is_sorted (Array.of_list xs);;

let find dict word = 
  let l = 0 in
  let r = Array.length dict - 1 in
  let rec binarySearch l r dict word = 
    print_string "\nleft: ";print_int l; print_string "\nright: ";print_int r;
    if l > r then -1 else
      let pos = (l + r) / 2 in
      print_string "\npos: ";print_int pos;
      let cp = String.compare word dict.(pos) in
      print_string "\ncp: ";print_int cp;
      if cp = 1
      then binarySearch (pos + 1) r dict word
      else if cp = -1
      then binarySearch l (pos - 1) dict word
      else pos
  in 
  binarySearch l r dict word;;