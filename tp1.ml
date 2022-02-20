Random.self_init ()
(* Exercice 1 *)
let x = 4 + (6/2) * 5;;
let y = false;;
let a = not y;;
let b = (fun x -> if x then false else true ) a;;

let y = if a then 12 else 40;;

let coul y = match y with 
    | true -> 12
    | false -> 45;;

let fonction a = a false ;;

(*error : let x = 4. * 3;; *)
(*error : let funcint a = a+5;; *)
(*error let x = 2;; x 5;; *)

(* Exercice 2 : *)

let rec _ackerman m n = 
    print_string " Ackerman ";
    print_int m;
    print_string(" ");
    print_int n;
    print_newline();
    if m = 0 
    then n+1 
    else 
        if m>0 && n=0 
        then _ackerman(m-1) 1
        else _ackerman(m-1) (_ackerman(m) (n-1))
;;

(*
_ackerman 2 1;;
*)

let _print_int_fonct x =
    print_int x;
    print_newline();
    x
;;

(*print_int_fonct 12;;*)

let rec est_pair a = if a mod 2 = 0 then true else false ;;



let rec _sycaruse m k = 
    print_float(m);
    print_newline();
    if k = 0
    then m 
    else if est_pair k 
        then (_sycaruse m (k-1)) /. 2.
        else 3. *. (_sycaruse m (k-1)) +. 1.
;;



let est_pair n = n mod 2 =0;;

(* une version simple avec k qui décroit à 0 *)
(* NE PAS FAIRE TOURNER pour de grands k car inefficace, 
vous pouvez observer les appels récursifs avec 
#trace syracuse1 *)

let rec syracuse1 m k = 
	if k =0 then m
		else if (est_pair (syracuse1 m (k-1)))
			then (syracuse1 m (k-1))/2
			else 3*(syracuse1 m (k-1))+1
;;


(* on évite le double appel récursif avec un let 
Observez les appel récursifs ici aussi *)
let rec syracuse2 m k = 
	if k =0 then m
		else 
		let terme_prec = syracuse2 m (k-1) in
		if (est_pair terme_prec)
			then terme_prec/2
			else 3*terme_prec+1
;;

(*  une version récursive terminale avec i qui croit et affichage.
On l'appelle toujours sur m (de l'énoncé), k (de l'énoncé), 
0 (pour i) et m (pour initialiser la suite) *)
let rec syracuse3 m k i res = 
	print_int i; print_string " - ";
	print_int res; print_newline ();
	if i =k then res
		else 
		if (est_pair res)
			then syracuse3 m k (i+1) (res/2)
			else syracuse3 m k (i+1) (3*res+1)
;;

(* comme on ne veut pas avoir trente mille paramètres, 
	on cache syracuse3 en fonction locale a syracuse *)

let syracuse4 m k =
	let rec aux_syracuse i res = 
	print_int i; print_string " - ";
	print_int res; print_newline ();
	(* ici les valeurs de m et k sont celles de syracuse,
	en revanche i et res sont locales à aux_syracuse *)
	if i =k then res
		else 
		if (est_pair res)
			then aux_syracuse (i+1) (res/2)
			else aux_syracuse (i+1) (3*res+1)
	in aux_syracuse 0 m
;;
