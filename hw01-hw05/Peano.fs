(* Addition, subtraction, multiplication and involution for numbers Peano
Author: Kudryashova Anna 
expected time: 2 hours
real time: 2,5 hours *)

type Peano = Zero | S of Peano
let rec plus a b =
  match a with
  | Zero -> b
  | S a -> S(plus a b)
let rec minus a b =
  match a, b with
  | Zero, _ -> Zero
  | a, Zero -> a
  | S a, S b -> minus a b
let rec toString a =
  match a with
  | Zero -> 0
  | S a -> 1 + (toString a)
let rec multi a b =
  match a, b with
  | _, Zero -> Zero
  | Zero, _ -> Zero
  | a, S b -> plus (multi a b) a
let rec deg x y =
  match x, y with
  | _, Zero -> S Zero
  | Zero,_ -> Zero
  | x, S y -> multi (deg x y) x
  
[<EntryPoint>]
let main argv = 
  0
