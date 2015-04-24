module Calculator
open System

type Tree= Empty| Num of int| Op of string * Tree * Tree


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


  
let Calc (in_string: Tree) =
  
  let rec calc tr =
    match tr with
    | Empty -> failwith "Empty"
    | Num (number) -> number
    | Op (v, tree1, tree2) -> let a = calc tree1
                              let b = calc tree2
                              match v with
                              | "+" -> a + b
                              | "-" -> a - b
                              | "*" -> a * b
                              | "/" -> a / b
                              | "%" -> b % a 
                              | "^" ->
                                      let rec pow e s =
                                        match s with
                                        | 0 -> 1
                                        | 1 -> e
                                        | s -> e * (pow e (s - 1))
                                      pow a b
                              | _ -> failwith "incorect"
    
  calc in_string
  
  
  
 

(*[<EntryPoint>]
let main argv =
  let in_string = "1 + 2"
  let out_string = calculator(in_string)
  printf "%A\n" out_string

  0*)
