type 'a abr = Leaf | Node of 'a abr * 'a * 'a abr;;

(* Question 1 *)
let vide = Leaf;;

let trois_noeud = Node (Node (Leaf, 4, Leaf), 5, Node (Leaf, 6, Leaf));;

let trois_noeud_pas_abr = Node (Node (Leaf, 4, Leaf), 6, Leaf);;


(* Question 2 *)

let compare a b = a < b;;

let rec recherche f elmnt abr = 
    match abr with
    | Leaf -> false
    | Node(g, a, d) -> if a = elmnt 
                        then true 
                        else 
                            recherche f elmnt (if f elmnt a then d else g)
;;

let print_bool bool = if bool then "true" else "false" ;;


(* test : print_string(print_bool(recherche compare 7 trois_noeud));; *)

(* Question 3 *)
let rec insere f elmnt abr = 
    match abr with
    | Leaf -> Node(Leaf, elmnt, Leaf)
    | Node(g, a, d) -> insere f elmnt (if f elmnt a then d else g)
;;

(* Test : 
let add = insere compare 7 trois_noeud ;;

print_string(print_bool(recherche compare 7 add));;
*)

(* Question 5 : *)

let rec liste_triee f list = 
    match list with
    | x::y::t -> if f x y then liste_triee f (y::t) else false
    | x::[] -> true
    | [] -> true
;;

(* Question 6 : *)
let rec collecte abr = 
    match abr with
    | Leaf -> []
    | Node(g, a, d) -> collecte g@[a]@collecte d
;;
(* test 
print_string(print_bool(liste_triee compare (collecte trois_noeud)));;
*)

(* Question 7 : *)
let est_abr1 abr = liste_triee compare (collecte abr);;

(* Question 8 : *)
let rec check f abr =
    match abr with
    | Leaf -> true
    | Node(g, a, d) -> check f g && check f d && f a
;;
(*Question 9 : *)
let rec est_abr2 abr = 
    match abr with
    | Leaf -> true
    | Node(g, a, d) -> check (fun x -> x<a) g && check (fun x -> x>= a) d && est_abr2 g && est_abr2 d
;;
(*test : 
print_string(print_bool(est_abr2(trois_noeud)));;
*)

(* tracer les fonctions :
#trace est_abr2;;
#trace est_abr1;;
*)



let observe func abr = 
    let debut = Sys.time () in
    func abr;
    let fin = Sys.time () in
    fin -. debut
;;

let construit_abr_plein n =
  (* attention ici n est la profondeur *)
  let rec pow2 n = if n = 0 then 1 else (if (n mod 2=0) then 1 else 2) * (pow2 (n/2) * pow2 (n/2)) in
  let rec aux n pivot base=
  if n = 0 then Leaf
    else Node (aux (n-1) (pivot / 2) base, pivot / 2+base, aux (n-1) (pivot/2) (base+pivot/2) ) in
    aux n (pow2 n) 0
  ;;

let abrTest = construit_abr_plein 18;;


print_float(observe est_abr1 abrTest);;
print_newline();;
print_float(observe est_abr2 abrTest);;
print_newline();;