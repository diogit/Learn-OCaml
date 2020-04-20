let rec children_from_char m c =
  match m with 
  | [] -> None
  | (char, t)::xs -> if char = c then Some t else children_from_char xs c;; 

let rec update_children m c t = 
  match m with
  | [] -> [(c, t)]
  | (char, trie)::xs -> if char = c
      then (char, t)::xs
      else (char, trie)::update_children xs c t;;

let rec lookup trie w = 
  match trie with
  | Trie(opt, l) ->
      let len = String.length w in
      if len = 0 then opt else
        let exists = children_from_char l w.[0] in
        match exists with
        | None -> None
        | Some t -> lookup t (String.sub w 1 (len - 1))
;;

let rec insert trie w v = 
  let len = String.length w in
  match trie with
  | Trie(opt, children) ->
      if len = 0
      then 
        Trie(Some v, children)
      else
        let wtail = String.sub w 1 (len - 1) in
        match children with
        | [] -> Trie(opt, [(w.[0],
                            insert empty wtail v)])
        | l ->
            let exists = children_from_char l w.[0] in
            match exists with
            | None -> Trie(opt,
                           (w.[0], insert empty wtail v)
                           ::children)
            | Some t -> Trie(opt, update_children children w.[0]
                               (insert t wtail v))
;; 