(* ex. 31
   expected time: 3 hours
   real time: 1 hour
   Author: Kudryashova Anna
*)
open NUnit.Framework


type IGraph =
  interface
    abstract Number : int
    abstract Edge : int -> int -> bool
    end
  
// 25. Интерфейс полиморфного помеченного графа 
type IMarkGraph<'A> = 
  interface
    inherit IGraph
    abstract Val  : int -> 'A
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
      elif (graph.Edge i v) && (not array.[i]) then 
        array.[i] <- true
        nfrom i
        else ()
  nfrom v
  array.[v] <- false
  for i = 0 to (size - 1) do
    if (array.[i]) then list <- i :: list
  list

let array = Array2D.create 4 4 false 
Array2D.set array 0 2 true
Array2D.set array 1 0  true
Array2D.set array 1 3 true
Array2D.set array 0 3 true
Array2D.set array 2 0 true
Array2D.set array 2 1 true
let example = new GraphMatrix(array)

[<Test>]
let ``Matrix: access from 0`` () =
  Assert.AreEqual((NodeTo (example, 0)), [3; 2; 1])
[<Test>] 
let  ``Matrix: access from 3`` () =
    Assert.AreEqual((NodeTo (example, 3)), [])
[<Test>] 
let  ``Matrix: 1 access from`` () =
    Assert.AreEqual((NodeFrom (example, 1)), [2; 0])
[<Test>] 
let  ``Matrix: 0 access from`` () =
    Assert.AreEqual((NodeFrom (example, 0)), [2; 1])


[<TestFixture>]
type ``test for graph with list`` () =
  let list = [|[2]; [0;3]; [0]; [2] |] 
  let graphList = new GraphAdjacencyList(list)
  [<Test>] 
  member this.``List: access from 1`` () =
    Assert.AreEqual((NodeTo (graphList, 1)), [3; 2; 0])
  [<Test>] 
  member this.``List: access from 2`` () =
    Assert.AreEqual((NodeTo (graphList, 2)), [0])
  [<Test>] 
  member this.``List: 3 access from`` () =
    Assert.AreEqual((NodeFrom (graphList, 3)), [1])
  [<Test>] 
  member this.``List: 0 access from`` () =
    Assert.AreEqual((NodeFrom (graphList, 0)), [3; 2; 1])
      

  
[<EntryPoint>]         
let main argv = 
    
    0 
