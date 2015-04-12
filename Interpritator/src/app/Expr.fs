module Expr

open System.IO
open System
type Tree = Var of string| Num of int| Op of string * Tree * Tree


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



let rec toTree_n (file: StreamReader) =
  let stack = new Stack<Tree> ()
  let mutable out = ""

  let isOp op =
    match op with
      | "*" -> true
      | "/" -> true
      | "-" -> true
      | "+" -> true
      | "%" -> true
      | "^" -> true
      | _ -> false


  let connectTree op =
    if not stack.Empty then
      let t1 = stack.head
      ignore(stack.pop)
      let t2 = stack.head
      ignore(stack.pop)
      let con = Op(op, t2, t1)
      stack.push (con)
  
  out <- file.ReadLine ()
  if (isOp out) then
    if (out.[0] = '-') && (out.Length - 1 > 1) then
      if Char.IsDigit(out.[1]) then
        let mutable number = []
        number <- [out;]
        let v = number.[0]
        stack.push (Num(Convert.ToInt32(v.ToString(), 10)))
    else  let t1 = toTree_n file
          let t2 = toTree_n file
          stack.push (t1)
          stack.push (t2)
          connectTree out
  else
    if Char.IsDigit(out.[0]) then
      let mutable number = []
      number <- [out;]
      let v = number.[0]
      stack.push (Num(Convert.ToInt32(v.ToString(), 10)))
    else stack.push (Var(out))
  
         
  stack.head




let Calc (in_string: Tree, vars: int list) =
  
  let rec calc tr =
    match tr with
    
    | Num (number) -> number
    | Var(var) -> let mutable result = 0
                  if (not vars.IsEmpty) then result <- vars.Head
                                             
                  result
                    
                   
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
                              | _ -> 000
    
  calc in_string
