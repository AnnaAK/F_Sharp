module Tree02

(* Exercise 14 - 17 from homework dated March 2
Author : Kudryashova Anna
Expected time: 4 hours
Real time:  8 hours *)

type Trees<'A> = Empty | Tree of 'A * Trees<'A> * Trees<'A>
 

let  rec insert e t =
  match e, t with
  | e, Empty -> Tree(e, Empty, Empty)
  | e, Tree(p, lc, rc) -> 
  if e > p then Tree(p, lc, insert e rc)
  else if e < p then Tree (p, insert e lc, rc)
  else t


let rec map f t =
  match t with
  | Empty -> Empty
  | Tree (p, lc, rc) -> Tree(f p, map f lc, map f rc)

let mapTree t = map (fun i -> i + 2.0) t

let rec fold f a t =
  match t with
  | Empty -> a
  | Tree (p, lc, rc) -> fold f (fold f (f a p) lc) rc
  
let sumTree t = fold (fun acc x -> acc + x) 0.0 t


[<EntryPoint>]
let main args =
  let  tr = Tree(9.0, Empty, Empty)
  printf "\n"
  let tr = insert 7.1 tr
  let tr = insert 8.4 tr
  let tr = insert 10.3 tr
  let tr = insert 5.7 tr
  let tr = insert 3.1 tr
  printf "original tree %A\n" tr
  printf "maping tree (i + 2): %A\n" (mapTree tr)
  printf "sum tree : %A\n" (sumTree tr)
  printf "\n"
  
  0
