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



let rec fold f1 f2 a tree  =
  match tree with
  | Empty -> a
  | Node(v, lc, rc) -> fold f1 f2 (fold f1 f2 (f1 (a) (f2 v)) lc) rc

let NewT newT oldT = fold insert (fun x -> x) newT oldT


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


