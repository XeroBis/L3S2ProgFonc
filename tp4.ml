Random.self_init();;
type face = Deux | Trois | Quatre | Cinq | Six | Sept | Huit | Neuf | Dix | Valet | Dame | Roi | As ;;

type couleur = Trefle | Carreau | Coeur | Pique ;;

type carte = Carte of face * couleur;;

let getFace i = 
    match i with 
    | 2 -> Deux 
    | 3 -> Trois 
    | 4 -> Quatre 
    | 5 -> Cinq 
    | 6 -> Six 
    | 7 -> Sept 
    | 8 -> Huit 
    | 9 -> Neuf 
    | 10 -> Dix 
    | 11 -> Valet 
    | 12 -> Dame 
    | 13 -> Roi 
    | 14 -> As 
    | _ -> failwith "error getFace";;

let getValue carte = 
    match carte with
        | Carte (Deux, _) -> 2
        | Carte (Trois, _) -> 3
        | Carte (Quatre, _) -> 4 
        | Carte (Cinq, _) -> 5
        | Carte (Six, _) -> 6
        | Carte (Sept, _ )-> 7
        | Carte (Huit, _ )-> 8
        | Carte (Neuf, _) -> 9
        | Carte (Dix, _) -> 10
        | Carte (Valet,_ )-> 11
        | Carte (Dame,_ )-> 12
        | Carte (Roi,_ )-> 13
        | Carte (As,_) -> 14;;




let compare c1 c2 =
    let v1 = getValue c1 in
    let v2 = getValue c2 in
    if v1 > v2 
    then
        1
    else
        begin
        if v1 == v2
        then
            0
        else 
            2
        end;;


let score_carte carte =
    match carte with
        | Carte (Deux, _) -> 0
        | Carte (Trois, _) -> 0
        | Carte (Quatre, _) -> 0
        | Carte (Cinq, _) -> 0
        | Carte (Six, _) -> 0
        | Carte (Sept, _ )-> 0
        | Carte (Huit, _ )-> 0
        | Carte (Neuf, _) -> 0
        | Carte (Dix, _) -> 10
        | Carte (Valet,_ )-> 2
        | Carte (Dame,_ )-> 3
        | Carte (Roi,_ )-> 4
        | Carte (As,_) -> 11;;



let getValueOfFace f = 
    match f with
    | Deux -> "2"
    | Trois -> "3"
    | Quatre -> "4"
    | Cinq -> "5"
    | Six -> "6"
    | Sept -> "7"
    | Huit -> "8"
    | Neuf -> "9"
    | Dix -> "10"
    | Valet -> "Valet"
    | Dame -> "Dame"
    | Roi -> "Roi"
    | As -> "As"
;;

let getValueOfCouleur c = 
    match c with 
    | Carreau -> "Carreau"
    | Trefle -> "Trefle"
    | Coeur -> "Coeur"
    | Pique -> "Pique"

let print_carte carte =
    match carte with
    | Carte(f, c) -> print_string (getValueOfFace f) ; print_string " de " ;print_string (getValueOfCouleur c); print_newline();
;;
let create_deck ()= 
    let rec sub_create_deck nb i =
        match i with
        | 1 -> Carte (getFace nb,Carreau):: if nb == 2 then [] else sub_create_deck (nb-1) i
        | 2 -> Carte (getFace nb,Coeur):: if nb == 2 then sub_create_deck 14 (i-1) else sub_create_deck (nb-1) i
        | 3 -> Carte (getFace nb,Trefle):: if nb == 2 then sub_create_deck 14 (i-1) else sub_create_deck (nb-1) i
        | 4 -> Carte (getFace nb, Pique):: if nb == 2 then sub_create_deck 14 (i-1) else sub_create_deck (nb-1) i
        | _ -> failwith "errror create deck" 
    in sub_create_deck 14 4
;;


let deck = create_deck ();;

let rec print_list list =  
    match list with
    [] -> ()
    | e::l -> print_carte e  ; print_list l 
;;
(* 
print_list deck;; 
*)


let echange l i = 
    let elem = List.nth l i  in
    elem::(List.filter (fun x -> x<>elem) l)
;;


(* Question 3 *)

let rec melange l i n =
    if i = n
    then
        l
    else 
        melange (echange l (Random.int 52)) (i+1) n
;;

(* 
let l1 = melange deck 0 1000;;
print_newline();;
print_list l1;;
*)


(* Question 4 *)
let rec distribue d l1 l2 = 
    if List.length l1 = List.length d / 2
    then
        (l1,l2)
    else 
        match d with
            | [] -> failwith "error"
            | x::y::t -> distribue d (x::l1) (y::l2)
;;

(* Question 5*)
