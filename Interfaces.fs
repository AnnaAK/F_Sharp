(* Author: Kudryashova Anna
 Expected time: 3 hours
 Real time: 2 hours *)
 
 // 20. Интерфейс ориентированного графа
 type IGraph<'A> =
  interface
    abstract Number : unit -> int
    abstract Edge : int -> int -> bool
    abstract Val  : int -> 'A
  end
 
// 25. Интерфейс полиморфного помеченного графа 
type IGraph2<'A> = 
  interface
    inherit IGraph<'A>
    abstract PolyEdge : 'A -> 'A -> bool
  end
  
  // 27. Интерфейс для полиморфного списка
  type IList<'A> = 
  interface
    abstract AddBegin  : 'A -> IList<'A>
    abstract AddEnd    : 'A -> IList<'A>
    abstract AddNumber : 'A -> int -> IList<'A>
    abstract RemBegin   : unit -> IList<'A>
    abstract RemEnd    :  unit -> IList<'A>
    abstract RemNumber : int -> IList<'A>
    abstract Search    : ('A -> bool) -> Option<'A>
    abstract Concat    : IList<'A> -> IList<'A>
  end
