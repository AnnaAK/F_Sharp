module Stmt
open Expr
open System
open System.IO

type tree = 
       | Read   of string
       | Write  of Tree
       | Assign of string * Tree
       | Seq    of tree * tree
       | If     of Tree * tree * tree
       | While  of Tree * tree
       | Empty 

(*let read (s: StreamReader) =
  let mutable result = ""
  result <- s.ReadLine()
  Read (result)

let assign (s: StreamReader) =
  let mutable result = ""
  result <- s.ReadLine()
  Assign(result, toTree_n s)

let write (s: StreamReader) =
  let mutable result = ""
  result <- s.ReadLine()
  Write(toTree_n s)*)


let toTree (in_string : string) = 
  use input = new StreamReader (in_string)
  let rec Parser () = 
    let mutable out = ""
    let mutable result = Empty
    out <- input.ReadLine()
    if (out <> null) then
      result <- match out with
                | "read" -> read (input)
                | ";" -> let t1 = Parser ()
                         let t2 = Parser ()
                         Seq (t1, t2)
                | ":=" -> assign(input)
                | "write" -> write (input)
                | "while" -> let t = Parser() 
                             While (toTree_n input, t)
                | "if" -> let then_ = Parser()
                          let else_ = Parser()
                          If (toTree_n input, then_, else_)
                | _ -> Empty
                
    result
  Parser ()


  


[<EntryPoint>]
let main argv =
  let out = toTree "MyTest.txt"
  printf "%A" out
  

  0
