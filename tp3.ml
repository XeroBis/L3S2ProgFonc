Random.self_init ()
(* Exercice 2 

let big  b f1 a f b2 a2 = if b && f1 = 1. && a == a2 && f 2 true == "foobar" then failwith "foo" else failwith "bar";;

*)


type face = Deux | Trois | Quatre | Cinq | Six | Sept | Huit | Neuf | Dix | Valet | Dame | Roi | As ;;

type couleur = Trefle | Carreau | Coeur | Pique ;;

type carte = Carte of face * couleur;;

let roipique = Carte (Roi, Pique);;

let ascoeur = Carte (As, Coeur);;


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


let getCarte i = 
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
    | _ -> failwith "error";;

let getCouleur i =
    match i with 
    | 1 -> Pique
    | 2 -> Carreau
    | 3 -> Coeur
    | 4 -> Trefle
    | _ -> failwith "error";;



let tire_random () = 
    let face = 2 + Random.int 13 in
    let couleur = 1+ Random.int 4 in
    Carte (getCarte face, getCouleur couleur);;



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


(* Question 8 :  *)
type joueur = Joueur of String * int;;

let add_score c1 c2 =
    score_carte c1 + score_carte c2;;

let change_score_joueur joueur score = 
    match joueur with 
    | Joueur (x, s) -> Joueur (x, s+score)
    | _ -> failwith "error bizarre";;

let getscore j = 
    match j with
    | Joueur(x,s) -> S
    | _ -> failwith "error joueur";;

let get_nom j = 
    match j with 
    | Joueur (x,_) -> x 
    | _ -> failwith "error nom joueur" ;;


let print_gagnant j1 j2 = 
    print_string (if getscore j1 > getscore j2 then get_nom j1 else begin if getscore j2 > getscore j1 then get_nom j1 else "Egalité !") ;;

let rec bataille j1 j2 tour =

    if tour < 10 
    then
        let carte1 = tire_random() in
        let carte2 = tire_random() in
        let winner = compare carte1 carte2 in
        begin
        if winner = 0
        then
            bataille j1 j2 (tour+1)
        else
            bataille change_score_joueur (if winner = 1 then j1 else j2) add_score carte1 carte2 (if winner = 1 then j2 else j1)  (tour +1) 
        end
    else
        print_gagnant j1 j2;;





(****** Question 9 Il nous manquerais des type comme les tableau en java, c++ *****)
            




