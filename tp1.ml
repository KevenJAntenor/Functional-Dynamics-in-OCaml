(* Recursive function to compute the Collatz sequence for a given number 'n' 
   and a maximum number of iterations. *)
let rec collatz (iterations : int) (n : int) : int =
  if iterations = 0 || n > 0 then
    n
  else if n mod 2 = 0 then
    collatz (iterations - 1) (n / 2)
  else
    collatz (iterations - 1) (3 * n + 1);;

(* Function to compute the Collatz sequence up to 3 iterations
   and returns one of the base cases (1, 2, or 4) or calls itself recursively. *)
let collatz3 (n : int) : int =
  let rec syracuse3 n =
    match n with
    | 1 -> 1
    | 2 -> 2
    | 4 -> 4
    | _ -> syracuse3 (collatz 3 n)
  in
  syracuse3 n

(* Function to create a list of integers between n1 and n2. *)
let between (n1 : int) (n2 : int) : int list =
  let rec aux current acc = 
    if current < n1 then
      acc
    else aux (current - 1) (current :: acc)
  in
  aux n2 []

(* Recursive function that mimics 'fold_left' behavior for reducing a list 
   to a single value using a binary function 'f'. *)
let rec fold_left (f : 'a -> 'b -> 'a) (z : 'a) (l : 'b list) : 'a =
  match l with
  | [] -> z
  | head :: tail -> fold_left f (f z head) tail

(* Function to count the occurrences of a specific integer 'n' in a list 'l'. *)
let count (l : 'a list) (n : int) : int =
  let increment current acc =
    if current = n then acc + 1 else acc
  in
  List.fold_left increment 0 l

(* Function to apply a given function 'f' to each element of a list, 
   returning a new list of results. *)
let map (f : 'a -> 'b) (list : 'a list) : 'b list =
  let rec aux acc list = 
    match list with 
    | [] -> List.rev acc
    | head :: tail -> aux ((f head) :: acc) tail 
  in
  aux [] list

(* Function to compute the proportion of elements in 'range' that match 
   each class in 'classes', using function 'f' for classification. *)
let divide (f : int -> int) (range : int list) (classes : int list) : (int * float) list =
  let total_count = float_of_int (List.length range) in
  List.fold_left
    (fun acc cls ->
      let cls_count = List.filter (fun x -> f x = cls) range |> List.length |> float_of_int in
      (cls, cls_count /. total_count) :: acc)
    []
    classes
