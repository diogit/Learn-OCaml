(* 21 - Escreva as seguintes três funções sobre árvores:
howMany : 'a -> 'a tree -> int - Conta número de ocorrências do valor na árvore.
eqPairs : ('a * 'a) tree -> int - Conta número de pares com as duas componentes iguais.
treeToList : 'a tree -> 'a list - Converte árvore em lista, por uma ordem qualquer. *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree;;

let rec howMany n t =
  match t with
  | Nil -> 0
  | Node(v, l, r) -> (if v = n then 1 else 0) + howMany n l + howMany n r
;;
howMany 1 Nil = 0;;
howMany 2 (Node(1, Node(1, Nil, Nil), Node(3, Nil, Nil))) = 0;;
howMany 3 (Node(1, Node(2, Nil, Nil), Node(3, Nil, Nil))) = 1;;

let rec eqPairs (lnode, rnode) =
  match lnode, rnode with
  | _, Nil -> 0
  | Nil, _ -> 0
  | Node(lv, ll, lr), Node(rv, rl, rr) -> (if lv = rv then 1 else 0) + eqPairs (ll, lr) + eqPairs (rl, rr)
;;
eqPairs (Nil, Nil) = 0;;
eqPairs (Node(2, Nil, Nil), Node(3, Nil, Nil)) = 0;;
eqPairs (Node(3, Nil, Nil), Node(3, Nil, Nil)) = 1;;
eqPairs (Node(3, Node(2, Nil, Nil), Node(2, Nil, Nil)), Node(3, Nil, Nil)) = 2;;

let rec treeToList t =
  match t with
  | Nil -> []
  | Node(v, l, r) -> v :: treeToList l @ treeToList r
;;
treeToList Nil = [];;
treeToList (Node(1, Nil, Nil)) = [1];;
treeToList (Node(1, Node(2, Nil, Nil), Node(3, Nil, Nil))) = [1;2;3];;
treeToList (Node(1, Node(2, Node(4, Nil, Nil), Node(5, Nil, Nil)), Node(3, Nil, Nil))) = [1;2;4;5;3];;

(* 22 - Programe uma função:
      balanced: 'a tree -> bool
que determine se uma árvore binária está ou não equilibrada.
Uma árvore binária diz-se equilibrada se, para cada nó, a diferença de profundidades das suas subárvores não superar a unidade.
(Copie a função a função auxiliar height da teórica 4.) *)

(* height: 'a tree -> int *)
let rec height t =
    match t with
       Nil -> 0
     | Node(x,l,r) ->
           1 + max (height l) (height r)
;;

let rec balanced t = 
  match t with
  | Nil -> true
  | Node(_, l, r) -> abs (height l - height r) <= 1;;
;;
balanced Nil = true;;
balanced (Node(3,Node(5,Nil,Nil),Node(6,Nil,Nil))) = true;;
balanced (Node(1,Nil,Node(2,Nil,Node(3,Nil,Nil)))) = false;;
balanced (Node(1,Node(2,Nil,Node(3,Nil,Nil)), Nil)) = false;;
balanced (Node(1,Node(2,Nil,Node(3,Node(4,Nil,Nil),Nil)), Node(2,Nil,Nil))) = false;;

(* 23 - Programe uma função:
      subtrees: 'a tree -> 'a tree list
que produza a lista de todas as subárvores distintas que ocorrem na árvore argumento. *)

let rec subtrees t =
  match t with
  | Nil -> [Nil]
  | Node(v, l, r) -> [Node(v, l, r)] @ subtrees l @ subtrees r
;;
subtrees (Node(5,Nil,Node(6,Nil,Nil))) = [Node(5,Nil,Node(6,Nil,Nil)); Node(6,Nil,Nil); Nil];;
subtrees Nil = [Nil];;


(* 24 - Primavera. Programe uma função:
      spring: 'a -> 'a tree -> 'a tree
que faça crescer novas folhas, com o valor do primeiro argumento, em todos os pontos duma árvore onde esteja Nil. Portanto, as novas folhas ficam todas iguais entre si. *)

let rec spring leaf t =
  match t with
  | Nil -> Node(leaf, Nil, Nil)
  | Node(v, l, r) -> Node(v, spring leaf l, spring leaf r)
;;
spring 1 Nil = Node(1, Nil, Nil);;
spring 9 (Node(1, Node(2, Nil, Nil), Node(3, Nil, Nil))) = Node(1, Node(2, Node(9, Nil, Nil), Node(9, Nil, Nil)), Node(3, Node(9, Nil, Nil), Node(9, Nil, Nil)));;

(* 25 - Outono. Programe uma função:
      fall: 'a tree -> 'a tree
que elimine todas as folhas existentes duma árvore. Os nós interiores que ficam a descoberto tornam-se folhas, claro, as quais já não são para eliminar. *)

let rec fall t =
  match t with
  | Nil -> Nil
  | Node(v, l, r) -> if l = Nil || r = Nil then Nil else Node(v, fall l, fall r)
;;
fall Nil = Nil;;
fall (Node(1, Nil, Nil)) = Nil;;
fall (Node(1, Node(2, Node(4, Nil, Nil), Node(5, Nil, Nil)), Node(3, Nil, Nil))) = Node(1, Node(2, Nil, Nil), Nil);;
fall (Node(1, Node(2, Node(9, Nil, Nil), Node(9, Nil, Nil)), Node(3, Node(9, Nil, Nil), Node(9, Nil, Nil)))) = Node(1, Node(2, Nil, Nil), Node(3, Nil, Nil));;

(* 26 - Repita os exercícios 23, 24 e 25, mas agora para árvores n-árias. O tipo árvore n-ária em OCaml define-se assim:
treeToList : 'a ntree -> 'a list
subtrees : 'a ntree -> 'a ntree list
spring: 'a -> 'a ntree -> 'a ntree
fall: 'a ntree -> 'a ntree
Nota: Uma folha duma árvore binária tem dois Nil por baixo, mas uma folha duma árvore n-ária costuma ter apenas uma lista vazia de filhos, portanto sem qualquer Nil. *)

type 'a ntree = NNil | NNode of 'a * 'a ntree list;;

let rec treeListToList tl =
  match tl with
  | [] -> []
  | n::ns -> (treeToList n) @ treeListToList ns
;;

let rec treeToList t =
  match t with
  | NNil -> []
  | NNode(v, tl) -> v :: (treeListToList tl)
;;

treeToList NNil = [];;
treeToList (NNode(1, [])) = [1];;
treeToList (NNode(1, [NNode(2, []); NNode(3, [])])) = [1;2;3];;
treeToList NNode(1, [NNode(2, [NNode(3, [])])]) = [1;2;3];;

(* 27 - Defina um tipo soma exp para representar expressões algébricas com uma variável real "x".
Nas expressões devem poder ocorrer os operadores binários "+", "-", "*", "/", o operador unário "-", a variável "x" e ainda literais reais. Exemplos de expressões:
2*x^2+5
2*x^7+5*(x-5)^2-3 *)

type exp =
  | Add of exp * exp   (* soma *)
  | Sub of exp * exp   (* subtração *)
  | Mult of exp * exp  (* multiplicação *)
  | Div of exp * exp   (* divisão *)
  | Power of exp * int (* potência *)
  | Sym of exp         (* simétrico *)
  | Const of float     (* constante*)
  | Var                (* variável *)
;;

(* 27 - a) eval: float -> exp -> float - Avalia a expressão para um dado valor de "x" *)
let rec eval v exp =
  match exp with
  | Add(e1, e2) -> eval v e1 +. eval v e2
  | Sub(e1, e2) -> eval v e1 -. eval v e2
  | Mult(e1, e2) -> eval v e1 *. eval v e2
  | Div(e1, e2) -> eval v e1 /. eval v e2
  | Power(e1, n) -> eval v e1 ** float_of_int n
  | Sym(e) -> Float.neg (eval v e)
  | Const(c) -> c
  | Var -> eval v (Const(v))
;;
eval 3.0 (Add(Const(2.0), Var)) = 5.0;;
eval 3.0 (Sub(Const(2.0), Var)) = -1.0;;
eval 3.0 (Mult(Const(2.0), Var)) = 6.0;;
eval 4.0 (Div(Const(2.0), Var)) = 0.5;;
eval 2.0 (Power(Var, 2)) = 4.0;;
eval 2.0 (Sym(Var)) = -2.0;;
eval 3.0 (Const(2.0)) = 2.0;;
eval 3.0 Var = 3.0;;

(* 27 - b) deriv: exp -> exp - Determina a derivada em ordem a "x". Não simplifique o resultado. *)
let rec deriv exp =
  match exp with
  | Add(e1, e2) -> Add(deriv e1, deriv e2)
  | Sub(e1, e2) -> Sub(deriv e1, deriv e2)
  | Mult(e1, e2) -> Add(Mult(deriv e1, e2), Mult(e1, deriv e2))
  | Div(e1, e2) -> Div(Sub(Mult(deriv e1, e2), Mult(e1, deriv e2)), Power(e2, 2))
  | Power(e1, n) -> Mult(Const(float_of_int n), Power(e1, n - 1))
  | Sym(e) -> Sym(deriv e)
  | Const(_) -> Const(0.0)
  | Var -> Const(1.0)
;;

deriv (Const(1.0)) = Const(0.0);;
deriv Var = Const(1.0);;
deriv (Sym(Var)) = Sym(Const(1.0));;
deriv (Add(Const(1.0), Const(2.0))) = Add(Const(0.0), Const(0.0));;
deriv (Add(Var, Const(3.0))) = Add(Const(1.0), Const(0.0));;
deriv (Sub(Const(1.0), Const(2.0))) = Sub(Const(0.0), Const(0.0));;
deriv (Sub(Var, Const(3.0))) = Sub(Const(1.0), Const(0.0));;
deriv (Mult(Const(2.0), Var)) = Add(Mult(Const(0.0), Var), Mult(Const(2.0), Const(1.0)));;
deriv (Div(Const(2.0), Var)) = Div(Sub(Mult(Const(0.0), Var), Mult(Const(2.0), Const(1.0))), Power(Var, 2));;
deriv (Power(Var, 2)) = Mult(Const(2.0), Power(Var, 1));;
deriv (Add(Mult(Const 2.0, Power(Var, 2)), Const 5.0)) = Add(Add(Mult(Const(0.0), Power(Var, 2)), Mult(Const 2.0, Mult(Const(2.0), Power(Var, 1)))), Const(0.0));;