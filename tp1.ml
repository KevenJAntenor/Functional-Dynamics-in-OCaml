(* Cours      : Programmation fonctionnelle et logique *)
(* Sigle      : INF6120 *)
(* Groupe     : 020 *)
(* Session    : Été 2024 *)
(* Auteur(s)  : <Keven Jude Anténor> (<ANTK08129003>) *)

(* La fonction collatz applique la suite de collatz 
à un nombre n pour un certain nombre d'itérations. *)

let rec collatz (iterations : int) (n : int) : int =
  if iterations = 0 then n
  else if n mod 2 = 0 then collatz (iterations - 1) (n / 2)
  else collatz (iterations - 1) (3 * n + 1)

(* La fonction collatz3 applique la suite de Collatz à un nombre n 
jusqu'à ce qu'il atteigne 1, 2 ou 4. *)
let rec collatz3 (n : int) : int =
    match collatz 3 n with
    | 1 -> 1
    | 2 -> 2
    | 4 -> 4
    | _ -> collatz3 (collatz 3 n)


(* La fonction between crée une liste de nombres allant de i à j *)
let  between (i : int) (j : int) : int list =
      let rec aux acc i j =
        if i > j then List.rev acc
        else aux (i :: acc) (i + 1) j
      in
      aux [] i j

(* La fonction count compte combien de fois un élément n apparaît dans une liste *)
let count (l : 'a list) (n : 'a) : int =
  List.fold_left (fun acc x -> if x = n then acc + 1 else acc) 0 l

(* La fonction map applique une fonction f à chaque élément d'une liste et retourne 
une nouvelle liste avec les résultats *)
let  map (f : 'a -> 'b) (list : 'a list) : 'b list =
    let rec aux f list acc =
      match list with
      | [] -> List.rev acc
      | x :: xs -> aux f xs (f x :: acc)
    in
    aux f list []

(* La fonction divide calcule la proportion d'occurrences de chaque classe dans une liste
 transformée par une fonction f *)
let divide (f : int -> int) (range : int list) (classes : int list) : (int * float) list =
    let total_count = float_of_int (List.length range) in
      List.map (fun cls ->
        let cls_count = List.filter (fun x -> f x = cls) range |> List.length |> float_of_int in
        (cls, cls_count /. total_count)) classes