
(* Question 1 *)
let racine_option (x:float) : float option = 
    if x >= 0.
    then
        Some (sqrt(x))
    else
        None
;;

let print_float_option (x : float option) : unit = 
    match x with
    None -> print_string("x was negative.")
    | Some s -> print_string("sqrt(x) = " ^ string_of_float(s))
;;

(* Question 2 *)
let div_option (x:float) (y:float) : float option = 
    if y != 0.
    then
        Some (x/.y)
    else 
        None
;;


(*Question 3 *)
let bind (x : 'a option) (f: 'a -> 'b option) : 'b option = 
    match x with 
        | None -> None
        | Some s -> f s
;;

(* Question 4 *)
let compose (f:'a -> 'b option) (g:'b -> 'c option) : 'a -> 'c option =
    match f with 
        | None -> None
        | Some s -> g s 
;;






(* test des appels : 

print_float_option(racine_option (-24.2) );;

print_newline() ;; *)
