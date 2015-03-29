(* ex. 20 - 24
 Authtor: Kudryashova Anna
 Expected time: 5
 Real time: 4,5
*)

type IGraph =
  interface
    abstract Number : int
    abstract Edge : int -> int -> bool
    end
  
type IGraph2<'A> = 
  interface
    inherit IGraph
    abstract Val  : int -> 'A
    abstract PolyEdge : 'A -> 'A -> bool
  end
  


  
type GraphMatrix(array: bool [,]) =
  class
    let matrix = Array2D.length1 array
    interface IGraph with
      member this.Number = matrix
      member this.Edge i j = array.[i,j] 
  end

type GraphAdjacencyList(array : int list []) =
  class
    let l = array.Length 
    interface IGraph with
      member this.Number = l
      member this.Edge i j  =
        let rec Find i e  =
          match e with
          | []     -> false
          | x :: e -> if x = i then true
                      else Find i e
        Find j array.[i]
                         
  end
  
  
let NodeTo (graph: IGraph, v) =
  let size = graph.Number
  let array = Array.create size false
  let mutable list = []
  array.[v] <- true
  let rec nto v =
    for i = 0 to (size - 1) do
      if (graph.Edge v i) && (not array.[i]) then 
        array.[i] <- true
        nto i
        else ()
  nto v
  array.[v] <- false
  for i = 0 to (size - 1) do
    if (array.[i]) then list <- i :: list
  list
  
  
let NodeFrom (graph: IGraph, v) =
  let size = graph.Number
  let array = Array.create size false
  let mutable list = []
  array.[v] <- true
  let rec nfrom v =
    for i = 0 to (size - 1) do
      if (i = v) then ()
      elif (graph.Edge v i) && (not array.[i]) then 
        array.[i] <- true
        nfrom i
        else ()
  nfrom v
  array.[v] <- false
  for i = 0 to (size - 1) do
    if (array.[i]) then list <- i :: list
  list

    
[<EntryPoint>]         
let main argv = 
    let array = Array2D.create 4 4 false 
    Array2D.set array 0 1 true
    Array2D.set array 0 3 true
    Array2D.set array 1 2  true
    Array2D.set array 2 3 true
    Array2D.set array 3 2 true
    

    let list = [| [ 1; 2]; [4]; [4]; [0;2]; [2] |]
    printfn " Graph with matrix: %A\n" array
    printfn " Graph adjacency list: %A\n" list

    let graphArr = new GraphMatrix(array) 
    let graphA = graphArr  :> IGraph 

    let graphList = new GraphAdjacencyList(list) 
    let graphL = graphList  :> IGraph 
    
    printfn "3 connect with: \n" 
    printfn "Graph with matrix : %A, \nGraph adjacency list : %A"
         (NodeTo (graphA, 3))  (NodeTo (graphL, 3))
         
    printfn "0 access from: \n" 
    printfn "Graph with matrix : %A, \nGraph adjacency list : %A"
         (NodeFrom (graphA, 0))  (NodeFrom (graphL, 0))
         
    0 
