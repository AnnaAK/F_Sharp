(* binary search tree
 Author: Kudryashova Anna *)

type Trees = EmptyTree | Tree of int * Trees * Trees
let  rec insert e t =
  match e, t with
  | e, EmptyTree -> Tree(e, EmptyTree, EmptyTree)
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
    | EmptyTree, EmptyTree -> EmptyTree
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
                       
   
[<EntryPoint>]
let main args =
  let  tr = insert 8 EmptyTree
  let tr = insert 3 tr
  let tr = insert 10 tr
  let tr = insert 4 tr
  let tr = remove 4 tr
  LCR tr
  printf "\n"
  LRC tr
  printf "\n"
  CLR tr
  printf "\n"
  0
