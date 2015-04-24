module Interpritator
open Stmt
open Calculator2
open System
open System. IO

type Stack<'A> () =
  class
    let mutable stack = []: 'A list
  
    member this.push el =
      stack <- el :: stack
    member this.head =
      match stack with
      | [] -> failwith "Empty stack"
      | hd :: l -> hd
    member this.pop  =
      match stack with
      |[] -> failwith "Stack is empty"
      |x :: body -> stack <- body
    member this.print =
      let rec print pnt =
        match pnt with
        | [] -> printfn ""
        | x :: body -> 
            printf "%A " x
            print body
      print stack
    member this.Empty = 
      match stack with
      | [] -> true
      | _ -> false
  end



let interpritator (in_string: string) (input: Strm) =
  let stack = ValofVar<string, int>()
  let tree = toTree (in_string)
  let out = new Stack<int> ()
  
  let rec interTree tree =
    match tree with
    
    | Seq (command, next) ->
                            match command with            
                            | Read(value) ->
                                            match input with
                                            | List (list) -> stack.add value (list.head)
                                                             ignore(list.pop)
                                            | Consl (file) -> use stream = new StreamReader(file)
                                                              stack.add value (Convert.ToInt32(stream.ReadLine()))
                            | Write(expr) -> 
                                            out.push(calculate expr stack)
            
                            | Assign(value, expr) -> stack.add value (calculate expr stack)
            
                            | If(expr, then_, else_) ->
                                         if (calculate expr stack) > 0 then interTree then_
                                         else interTree else_
            
                            | While(expr, branch) ->
                                       while (calculate expr stack) > 0 do interTree branch
            
                            | _ -> ()
            
                            interTree next
        
     | _ -> ()

  interTree tree
  out

