
(* Exaples 9 - 13 from homework dated March 2
Author : Kudryashova Anna
Expected time: 3 hours
Real time:  3,5 hours *)

(* List.iter:
val it : (('a -> unit) -> 'a list -> unit) = <fun:clo@3> *)

let Reverse list = List.fold (fun acc i -> i :: acc) [] list

let filter f l = List.fold (fun acc x -> if f x then x :: acc else acc) [] l 
let filterF list = filter (fun i -> i < 8) list

let map f l = List.fold (fun acc x ->   f x :: acc) [] l
let mapF l = map (fun i -> i + 2) l

let Gorner l i = List.fold(fun acc x -> acc * i + x) 0 l

            

[<EntryPoint>]
let main args =
  let l = [1; 7; 8; 5; 4; 11]
  printf "original list: %A\n" l
  printf "reverse list: %A\n" (Reverse l)
  printf "i < 8\n"
  printf "filter: %A\n" (filterF l)
  printf "i + 2\n"
  let l = mapF l
  printf "map: %A\n" (Reverse l)
  let l = [4; 3; 2]
  printf "были введены коэфициенты (начиная со старшего) %A\n" l
  printf "значение переменной: 2\n"
  printf "gorner: %A\n" (Gorner l 2)
  0
