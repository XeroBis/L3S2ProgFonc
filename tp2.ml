
let lancer_de () = 1 + Random.int 6 ;;


let rec _jouerRec score = 
    let s = lancer_de() in
    print_int s;
    print_newline();
    if s > 4
    then (_jouerRec score) +  s
    else s
;;

let _jouer () = _jouerRec 0 ;;


let rec _tour_501 score = 
    print_string("SCORE :");
    print_int(score);
    print_newline();
    let s = lancer_de() in
    let x = s + lancer_de() in
    let y = x + lancer_de() in
    if y <= score 
    then score-y
    else 0 
;;



let rec _jouer_501 nbJoueur nbTour bestJoueur nbTourBestJoueur currentScore =
    if (nbJoueur = 0)
    then bestJoueur
    else
        let score = _tour_501 currentScore in
        if (score = 0)
        then if (nbTour > nbTourBestJoueur)
            then _jouer_501 (nbJoueur-1) 0 nbJoueur nbTour 501
            else _jouer_501 (nbJoueur-1) 0 bestJoueur nbTourBestJoueur 501
        else _jouer_501 nbJoueur (nbTour+1) bestJoueur nbTourBestJoueur score 
;;




let _jouer_really nbJoueur = _jouer_501 nbJoueur 0 5 0 501 ;;

print_int(_jouer_really 8);;
print_newline();

