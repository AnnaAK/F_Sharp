module test

open NUnit.Framework
open System.IO
open Stmt
open Interpritator
open Start

[<TestCase ("MyTest1.txt", [|4; 6;|], Result = "10")>]
let ``Test for interpreter`` (file : string) (args : int array) =


  let q = new toTree.Stack<int>()
  for l in args do q.push(l)
  let output = interpritator (makeSt(file)) (List q)
    
  let mutable result = ""
  while not output.Empty do
        result <- result + (output.head).ToString() + " "
        ignore(output.pop)
  result
