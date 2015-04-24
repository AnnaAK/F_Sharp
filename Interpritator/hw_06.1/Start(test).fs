module Start


open Interpritator
open System.IO
open toTree
open Stmt
open Calculator2
open NUnit.Framework

let makeSt (file : string) = 
   use inStream = new StreamReader(file)
   let str = inStream.ReadToEnd ()
   let mutable temp = ""
   let mutable out = ""
   for c in str do
        match c with
        | '\r' -> ()
        | '\n' -> 
            out <- out + temp; 
            temp <- ""
        |   c  -> temp <- temp + c.ToString()
   if temp <> "" then out <- out + temp
   out 
  

  




[<EntryPoint>]
let main argv = 
  let input = makeSt(argv.[0])
  if argv.Length < 2 then
        let output = interpritator input (Consl "")
        while not output.Empty do
            System.Console.Write(output.head)
            ignore(output.pop)
            printf "\n"
  else
        let output = interpritator input (Consl argv.[1]) 
        if argv.Length < 3 then 
            while not output.Empty do
                System.Console.Write(output.pop)
                ignore(output.pop)
        else
            use outputStream = new StreamWriter(argv.[2])
            while not output.Empty do
                outputStream.Write(output.head)
                ignore(output.pop)
  
  0

[<TestCase ("MyTest1.txt", [|4; 6;|], Result = "10")>]
let ``Test for interpreter`` (file : string) (args : int array) =


  let q = new Stack<int>()
  for l in args do q.push(l)
  let output = interpritator (makeSt(file)) (List q)
    
  let mutable result = ""
  while not output.Empty do
        result <- result + (output.head).ToString() + " "
        ignore(output.pop)
  result
