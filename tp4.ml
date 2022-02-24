type face = Deux | Trois | Quatre | Cinq | Six | Sept | Huit | Neuf | Dix | Valet | Dame | Roi | As ;;

type couleur = Trefle | Carreau | Coeur | Pique ;;

type carte = Carte of face * couleur;;


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

let rec ajout_face l i face= begin match i with
  | 1 -> l
  | _ -> [Carte (int_to_face i,face)]::(ajout_face l (i-1) face)@l
end;;

let rec ajout_carte l j = begin match j with
    | 1 -> (ajout_face l 14 Carreau)
    | 2 -> (ajout_face l 14 Coeur)@(ajout_carte l 1)@l
    | 3 -> (ajout_face l 14 Trefle)@(ajout_carte l 2)@l
    | 4 -> (ajout_face l 14 Pique)@(ajout_carte l 3)@l
    | _ -> failwith "Entier incorrect pour j"
end;;

let l1 = [];;
let deck = ajout_carte l1 4;;
