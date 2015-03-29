(* Author: Kudryashova Anna
 Expected time: 3 hours
 Real time: 2 hours *)
 
 
// 25. Интерфейс полиморфного помеченного графа 
type IMarkGraph<'A> = 
  interface
    inherit IGraph
    abstract Val  : int -> 'A
  end
  

