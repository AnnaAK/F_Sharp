module ex41

open System

type Tree<'A> = Empty| Node of int * Tree<'A> * Tree<'A>

let rec insert tree i =
  match i, tree with
  | i, Empty -> Node(i, Empty, Empty)
  | i, Node(v, lc, rc) ->
    if i = v then Node(v, lc, rc)
    else if i < v then Node(v, insert lc i, rc)
    else Node(v, lc, insert rc i)


let rec fold1 f a t =
  match t with
  | Empty -> a
  | Node (v, lc, rc) -> fold1 f (fold1 f (f a v) lc) rc

(*let rec fold f a tree  =
  match tree with
  | Empty -> a
  | Node(v, lc, rc) -> fold f (fold f (f (a v)) lc) rc*)

let rec fold f1 f2 a tree  =
  match tree with
  | Empty -> a
  | Node(v, lc, rc) -> fold f1 f2 (fold f1 f2 (f1 (a) (f2 v)) lc) rc

let NewT newT oldT = fold insert (fun x -> x) newT oldT


(*type NTree (tree:Tree<'A>) =
  class
     member this.add v =
      insert tree v
  end

let NewTree (tree: Tree<'A>) =
  let result = new NTree(Empty)
  let rec addEl tree =
    match tree with
    
    | Node(v, Empty, Empty) -> result.add v
    | Node (v, lc, rc) -> 
                          let a = addEl lc
                          let b = addEl rc
                          result.add v

    | Empty -> Empty
    
  
  printf "%A\n" result
  result*)


type TreeBuilder () =
  member this.Bind (x, rest) = fold NewT rest Empty x
  member this.Return x = Node (x, Empty, Empty)
  member this.Combine (a: Tree<'A>, b: Tree<'A>)=
    let rec comb t1 t2 =
      match t1 with
      | Empty -> t2
      | Node (v, lc, rc) -> match t2 with
                            | Empty -> t1
                            | Node (v2, lc2, rc2) -> Node (v, comb lc t2, rc)
    comb a b
    
    
                               
  member this.Delay f = f()
  

  member this.Yield x = x

let treeWF  = TreeBuilder()

(*let map f tree = 
    fold (fun t arg -> insert (f arg) t) Null tree*)


let mapTree f tr =
 treeWF {
   let! tree = tr
   return f tree
 }

let concatTree t1 t2 = 
  treeWF {
    yield t1
    yield t2
    
  }

let filterTree  f tr =
  treeWF {
    let! tree = tr
    return f tree


let filter f l = List.fold (fun acc x -> if f x then x :: acc else acc) [] l 
let filterF list = filter (fun i -> i < 3) list
(*let treeMap f t =
  treeWorkflow {
    for n in t do
      return f n
  }
  
[<EntryPoint>]
let main argv =
  let tree = Node(7, Node(10, Empty, Empty), Empty)
  let tree2 = Node (2, Node(8, Empty, Empty), Empty)
  let comb = concatTree tree tree2
  let res = mapTree( fun x -> x * 3) tree
  
  printf "%A\n" res
  printf "%A\n" comb


  0         
         *)
