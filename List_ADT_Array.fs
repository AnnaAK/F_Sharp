 (* ex 27 - 29
  Author: Kudryashova Anna
  Expected time: 5 hours
  Real time: 20 hours
 *)
 
type IList<'A> =
    interface
      abstract Length : int with get,set
      abstract AddBegin  : 'A -> unit
      abstract AddEnd    : 'A -> unit
      abstract AddNumber : 'A -> int -> unit
      abstract RemBegin   : unit -> unit
      abstract RemEnd    :  unit -> unit
      abstract RemNumber : int -> unit
      abstract Search    : ('A -> bool) -> Option<'A>
      abstract Concat    : IList<'A> -> unit
      abstract Head   : unit -> Option<'A>
      abstract Last   : unit -> Option<'A> 
      abstract Empty: unit -> bool
      abstract Print : unit -> unit
   end
   
type List_ADT<'A when 'A: equality> = Empty | Node of 'A * List_ADT<'A>
type List<'A when 'A: equality> () =
    class
        let mutable list = Empty
        let rec length list =
            match list with
            | Empty -> 0
            | Node (x, list) -> 1 + (length list)
        member this.Val  
                with  get () = list
                and set (value) = list <- value                               
                                  do (this :> IList<'A>).Length <- length list              
        interface IList<'A> with
            member val Length = 0 with get,set
            member this.AddBegin x  = 
                this.Val <- Node (x, this.Val) 
            member this.AddEnd x =
                let rec addE l x =
                    match l with
                    | Empty -> Node ( x, Empty)
                    | Node (head, l) -> Node ( head, (addE l x) )
                this.Val <- addE this.Val x
            member this.AddNumber x i =
              if i = 0 then (this :> IList<'A>).AddBegin(x)
                else
                    let rec push n l =
                        if n = i then Node (x, l)
                        else
                            match l with
                            | Empty -> failwith "not enough elements in list"
                            | Node (x, y) -> Node (x, push (n + 1) y)
                    list <- push 0 list
            member this.RemBegin () =
                match this.Val with
                | Empty -> failwith "list is empty"
                | Node (head, l) -> this.Val <- l
            member this.RemEnd () = 
                let rec removeE l =
                    match l with
                    | Empty -> failwith "not enoght elements in list"
                    | Node (x, Node (x', Empty)) -> Node (x, Empty)
                    | Node (x, y) -> Node (x, removeE y)
                list <- removeE list
            member this.RemNumber i = 
                if i = 0 then (this :> IList<'A>).RemBegin()
                else
                    let rec removeN n l =
                        match l with
                        | Empty -> failwith "not enough elements in list"
                        | Node (x, y) ->
                            if n = i - 1 then 
                                match y with
                                | Empty -> failwith "not enough elements in list"
                                | Node (_, y') -> Node (x, y')
                            else
                                Node (x, removeN (n + 1) y)
                    list <- removeN 0 list
            member this.Search func =
               let rec find f l = 
                   match l with
                   | Empty -> None
                   | Node (x, l) -> 
                       if f x then Some x
                       else find f l
               find func this.Val
            member this.Head () =
                match this.Val with
                | Empty -> None
                | Node (x, _) -> Some x
            member this.Last () = 
                let rec last l=
                    match l with
                    | Empty  -> None
                    | Node ( x , Empty) -> Some x
                    | Node (x, l)     -> last l
                last this.Val
            member this.Empty() = (this.Val = Empty)
            member this.Concat l = 
                let temp= new List<'A> ()
                temp.Val <- this.Val
                while l.Empty() = false do
                    let x = (l.Head ()).Value
                    (temp:> IList<'A>).AddEnd x
                    ignore (l.RemBegin () )
                this.Val <- temp.Val
            member this.Print () = 
                printfn "%A\n" this.Val
    end  
    
    type List_array<'A when 'A: equality> () =
    class
        let mutable list = [||]
        member this.Val  
                with  get () = list
                and set (value) = list <- value                               
                                  do (this :> IList<'A>).Length <- list.Length 
        interface IList<'A> with
            member val Length = 0 with get,set
            member this.AddBegin x  = 
                this.Val <- Array.append [|x|] this.Val
            member this.AddEnd x = 
                this.Val <- Array.append this.Val [|x|]
            member this.RemBegin () =
                let mas = this.Val
                let n = (this:> IList<'A>).Length
                match this.Val with
                | [| |] -> failwith "list is empty"
                |  _   ->  this.Val <-  Array.sub mas 1 (n - 1)
            member this.RemEnd () = 
                let n = (this:> IList<'A>).Length
                match this.Val with
                | [| |] -> failwith "list is empty"
                |  _   -> this.Val <- Array.sub this.Val 0  (n- 1)
            member this.Head () =
                match this.Val with
                | [||] -> None
                | _  ->  Some this.Val.[0]
            member this.Last () = 
                match this.Val with
                | [||] -> None
                | _  ->  Some this.Val.[(this:> IList<'A>).Length - 1]
            member this.Empty() = (this.Val = [||])
            member this.Concat (second: IList<'A> ) = 
                let temp= new List_array<'A> ()
                temp.Val <- this.Val
                while second.Empty() = false do
                    let x = (second.Head ()).Value
                    (temp:> IList<'A>).AddEnd x
                    ignore ( second.RemBegin ())
                this.Val <- temp.Val 
            member this.Print () = 
                printfn "%A\n" this.Val
    end      


[<EntryPoint>]
let main argv = 
    
    let list= new List<int> ()
    list.Val <- Node (0, Node (22, Empty) ) 
    printfn "List 1: "
    let ls = list:> IList<int>
    ls.Print ()
    printfn "Add 101 to beginning:"
    ls.AddBegin 101
    ls.Print ()
    printfn "Add 5 to the end:"
    ls.AddEnd 5
    ls.Print ()
    printfn "Add 0 to 2nd place: "
    ignore (ls.AddNumber 0 2)
    ls.Print ()
    printfn "Remove 1st element: "
    ignore (ls.RemBegin () )
    ls.Print ()
    printfn "Remove element in the end: "
    ignore (ls.RemEnd () )
    ls.Print ()
    printfn "Remove 2nd element: "
    ignore ( ls.RemNumber 2 )
    ls.Print ()
    printfn "Find 9:  %A " (ls.Search (fun x -> x = 9))
    let list2= new List<int> ()
    list2.Val <- Node (9, Node (0, Node ( 10, Empty) ) )
    let ls2 = list2:> IList<int>
    printfn "List 2:"
    ls2.Print ()
    printfn "Concat List 1 and List 2:"
    ls.Concat ls2
    ls.Print ()

    let array= new List_array<int> ()
    array.Val <- [|4; 5; 1; 6; 7|]
    printfn "Array 1:"
    let arr = array:> IList<int>
    arr.Print ()
    printfn "Add 45:"
    arr.AddBegin 45
    arr.Print ()
    printfn "Add 5 to the end:"
    arr.AddEnd 5
    arr.Print ()
    printfn "Remove head:"
    ignore (arr.RemBegin ())
    arr.Print ()
    printfn "Remove end:"
    ignore (arr.RemEnd () )
    arr.Print ()
    let array2= new List_array<int> ()
    array2.Val <- [| 1; 3; 6; 7|]
    let arr2 = array2:> IList<int>
    printfn "Array 2:"
    arr2.Print ()
    printfn "Concat:"
    arr.Concat arr2
    arr.Print ()
  0
    
 
