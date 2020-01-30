(*13 - Infira o tipo de cada uma das seguintes funções:*)

let f1 x = x + 1 ;;
(* int -> int *)

let f2 x = f1 x ;;
(* int -> int *)

let f3 x = 1 + x 5 ;;
(* (int -> int) -> int *)

let f4 x y = x < y x ;;
(* a' -> (a' -> a') -> bool *)

(* Para cada um dos tipos abaixo, dê também um exemplo duma função *)
(* (int->int) -> int *)
let f5 x = x 3 + 1;;

(* bool -> float -> string *)
let f6 b f = if b && (f > 1.0) then "yes" else "no";;

(* 14 - Escreva em OCaml uma função 
        succAll : int list -> int list
que produza a lista dos sucessores duma lista de inteiros. *)
let rec succAll l =
  match l with
  | [] -> []
<<<<<<< HEAD
  | x::xs -> x + 1 :: succAll xs
;;
=======
  | x::xs -> x + 1 :: succAll xs;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
succAll [] = [];;
succAll [3; 6; 1; 0; -4] = [4; 7; 2; 1; -3];;

(* 15 - Implemente o tipo de dados conjunto. Para representar os conjuntos, use listas não ordenadas mas sem repetições. As funções pretendidas são as seguintes:
      belongs: 'a -> 'a list -> bool - teste de pertença
      union: 'a list -> 'a list -> 'a list - união de conjuntos
      inter: 'a list -> 'a list -> 'a list - intersecção de conjuntos
      diff: 'a list -> 'a list -> 'a list - diferença de conjuntos
      power: 'a list -> 'a list list - conjunto potência: conjunto dos subconjuntos - difícil *)
let rec belongs el l =
  match l with
  | [] -> false
<<<<<<< HEAD
  | x::xs -> x = el || belongs el xs
;;
=======
  | x::xs -> x = el || belongs el xs;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
belongs 4 [1;2;3;4;5;6] = true;;
belongs 4 [1;2] = false;;

let rec union l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | l1, [] -> l1
  | [], l2 -> l2
  | _, x2::x2s -> if not (belongs x2 l1)
                  then union (l1@[x2]) x2s
<<<<<<< HEAD
                  else union l1 x2s
;;
=======
                  else union l1 x2s;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0

let rec union l1 l2 =
  match l1 with
  | [] -> l2
<<<<<<< HEAD
  | x::xs -> if belongs x l2 then union xs l2 else x::(union xs l2)
;;
=======
  | x::xs -> if belongs x l2 then union xs l2 else x::(union xs l2);;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
union [7;3;9] [2;1;9] = [7;3;2;1;9];;

let rec inter l1 l2 =
  match l1, l2 with
  | _, [] -> []
  | [], _ -> []
  | x1::x1s, x2::x2s -> if belongs x1 l2
                        then x1::(inter x1s x2s)
<<<<<<< HEAD
                        else inter x1s x2s
;;
=======
                        else inter x1s x2s;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0

let rec inter l1 l2 =
  match l1 with
  | [] -> []
<<<<<<< HEAD
  | x::xs -> if belongs x l2 then x::(inter xs l2) else inter xs l2
;;
=======
  | x::xs -> if belongs x l2 then x::(inter xs l2) else inter xs l2;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
inter [7;3;9] [2;1;9] = [9];;

let rec diff l1 l2 =
  match l1 with
  | [] -> []
<<<<<<< HEAD
  | x::xs -> if belongs x l2 then diff xs l2 else x::(diff xs l2)
;;
=======
  | x::xs -> if belongs x l2 then diff xs l2 else x::(diff xs l2);;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
diff [7;3;9] [2;1;9] = [7;3];;

let rec fept e t = 
  match t with
  | [] -> []
<<<<<<< HEAD
  | x::xs -> e::x::(fept e xs)
;;
=======
  | x::xs -> e::x::(fept e xs);;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
      
(* F (e, T) = {X ∪ {e} | X ∈ T} *)
let rec fept e t = 
  match t with
  | [] -> []
<<<<<<< HEAD
  | x::xs -> (union [e] x)::(fept e xs)
;;
=======
  | x::xs -> (union [e] x)::(fept e xs);;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0

(* If S = { }, then P(S) = { { } } is returned.
  else:
    Let e be any single element of S.
    Let T = S \ {e} be the set S with element e excluded.
    And the result: P(S) = P(T) ∪ F (e, P(T)) is returned. *)
let rec power l =
  match l with
  | [] -> [[]]
<<<<<<< HEAD
  | x::xs -> union (power xs) (fept x (power xs))
;;
=======
  | x::xs -> union (power xs) (fept x (power xs));;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
power [] = [[]];;
power [2;3] = [[]; [3]; [2]; [2; 3]];;
power [1;2;3] = [[]; [3]; [2]; [2; 3]; [1]; [1; 3]; [1; 2]; [1; 2; 3]];;

(* 16 - Escreva em OCaml uma função
      nat : int -> int list
que, dado um inteiro n, gere a lista dos n primeiros números naturais, ordenada decrescentemente. *)
let rec nat n =
  match n with
  | 0 -> []
  | n -> n - 1::nat (n - 1) ;;
<<<<<<< HEAD
let rec nat n = if n = 0 then [] else n - 1::nat (n - 1)
;;
=======
let rec nat n = if n = 0 then [] else n - 1::nat (n - 1);;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
nat 0 = [];;
nat 1 = [0];;
nat 2 = [1;0];;
nat 5 = [4;3;2;1;0];;

(* 17 - a) Escreva em OCaml uma função
      pack : 'a list -> ('a * int) list
para compactar listas, substituindo cada subsequência de valores repetidos por um par ordenado que indica qual o valor que se repete e qual o comprimento da subsequência. *)
let rec countEquals el n l =
  match l with
  | [] -> (el, n+1)::[]
<<<<<<< HEAD
  | x::xs -> if el = x then countEquals x (n+1) xs else (el, n+1)::countEquals x 0 xs
;;
=======
  | x::xs -> if el = x then countEquals x (n+1) xs else (el, n+1)::countEquals x 0 xs;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0

let rec pack l =
  match l with
  | [] -> []
  | [x] -> [(x, 1)]
<<<<<<< HEAD
  | x::xs -> countEquals x 0 xs
;;
=======
  | x::xs -> countEquals x 0 xs;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
pack [] = [];;
pack [10.1; 10.1; 10.1; 10.1; 10.1; 10.1; 10.1; 10.0; 10.0; 10.1; 10.0] =
[(10.1, 7); (10.0, 2); (10.1, 1); (10.0,1)];;

(* 17 - b) Escreva agora uma função para descompactar listas. 
      unpack : ('a * int) list -> 'a list *)
let rec helpUnpack el rep = if rep > 1 then el::helpUnpack el (rep - 1) else el::[];;

let rec unpack l = 
  match l with
  | [] -> []
<<<<<<< HEAD
  | (x, n)::xs -> helpUnpack x n @ unpack xs
;;
=======
  | (x, n)::xs -> helpUnpack x n @ unpack xs;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
unpack [] = [];;
unpack [(10.1, 7); (10.0, 2); (10.1, 1); (10.0,1)] =
[10.1; 10.1; 10.1; 10.1; 10.1; 10.1; 10.1; 10.0; 10.0; 10.1; 10.0];;

(* 18 - a) Escreva em OCaml uma função de ordem superior sobre listas, que permita aplicar sucessivamente uma operação binária associativa à esquerda a todos os elementos duma lista.
A função também precisa de receber o elemento neutro da operação.
      fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f neut l =
  match l with
  | [] -> neut
<<<<<<< HEAD
  | x::xs -> fold_left f (f neut x) xs
;;
=======
  | x::xs -> fold_left f (f neut x) xs;;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
fold_left (+) 0 [1;2;3;4] = List.fold_left (+) 0 [1;2;3;4];;
fold_left (-) 0 [1;2;3;4] = List.fold_left (-) 0 [1;2;3;4];;
fold_left (^) "" ["O";"C";"a";"m";"l"] = List.fold_left (^) "" ["O";"C";"a";"m";"l"];;
fold_left (fun x y -> y::x) [1;2] [3;4] = List.fold_left (fun x y -> y::x) [1;2] [3;4];;

(* 18 - b) Escreva uma função de ordem superior sobre listas, semelhante à anterior mas aplicável a operações binárias associativas à direita.
      fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f l neut =
  match l with
  | [] -> neut
<<<<<<< HEAD
  | x::xs -> f x (fold_right f xs neut)
;;
=======
  | x::xs -> f x (fold_right f xs neut);;
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0
fold_right (+) [1;2;3;4] 0 = List.fold_right (+) [1;2;3;4] 0;;
fold_right (-) [1;2;3;4] 0 = List.fold_right (-) [1;2;3;4] 0;;
fold_right (^) ["O";"C";"a";"m";"l"] "" = List.fold_right (^) ["O";"C";"a";"m";"l"] "";;
fold_right (fun x y -> x::y) [1;2] [3;4] = List.fold_right (fun x y -> x::y) [1;2] [3;4];;

<<<<<<< HEAD
(* 19 - Procure no manual de referência da linguagem OCaml (disponível na página da cadeira, na coluna da esquerda - veja "Basic Operations")
 a lista completa das operações associadas aos tipos bool, int, float, string e char. *)
=======
(* 19 - Procure no manual de referência da linguagem OCaml (disponível na página da cadeira, na coluna da esquerda - veja "Basic Operations") a lista completa das operações associadas aos tipos bool, int, float, string e char. *)
>>>>>>> 05ed5a8f83cffed024ab11a93e571c747c46f7b0

(* 20 - Para cada um dos seguintes tipos, invente uma função com esse tipo:
      int -> (int->int)
      (int->int) -> (int->int)
      int -> float -> string -> char *)
exception Not_Implemented;;
let f20 i = raise Not_Implemented;;
f20 0;;

let f21 fi = raise Not_Implemented;;
f21 0;;

let f22 i f s = String.get s (i + Int.of_float f);;
f22 1 1.0 "OCaml" = 'a';;