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
  
let rec FindR t =
  match t with
  | EmptyTree -> 0
  | Tree (p, EmptyTree, rc) -> p
  | Tree (p, lc, rc) -> FindR lc
  
let rec FindL t =
  match t with
  | EmptyTree -> 0
  | Tree (p, lc, EmptyTree) -> p
  | Tree (p, lc, rc) -> FindL rc
  
let rec remove e t =
  match t with
  |EmptyTree -> EmptyTree
  |Tree (p, lc, rc) ->
  if e > p then Tree(p, lc, remove e rc)
  else if e < p then Tree(p, remove e lc, rc)
  else  match lc, rc with
        |EmptyTree, EmptyTree -> EmptyTree
        | lc, EmptyTree -> lc
        | EmptyTree, rc -> rc
        | lc, rc -> if lc = EmptyTree then Tree(FindR rc, lc, remove (FindR rc) rc)
                    else Tree(FindL lc, remove (FindL lc) lc, rc)
                
    
let rec LCR t =
  match t with
  | EmptyTree -> printf ""
  | Tree (p, lc, rc) -> LCR lc
                        printf  "%d, " p
                        LCR rc
                        
let rec LRC t =
  match t with
  | EmptyTree -> printf ""
  | Tree (p, lc, rc) -> LRC lc
                        LRC rc
                        printf  "%d, " p
                        
let rec CLR t =
  match t with
  | EmptyTree -> printf ""
  | Tree (p, lc, rc) -> printf  "%d, " p
                        CLR lc
                        CLR rc
              


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

let minTree t =
  let min y x =
    match y with
    | None -> Some x
    | Some y -> Some (min y x)
  fold min None t 
let copyTree t = fold (fun acc x -> insert x acc) Empty t


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
  printf "min in tree: %A\n" (minTree tr)
  printf "copy of tree: %A" (copyTree tr)
  printf "\n"
  
  0
