(* ex. 37
   Author: Kudryashova Anna
*)

open System.IO
open NUnit.Framework
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

let convert (in_string : string) =
    let priority operator =
        match operator with
        | "+" -> 1
        | "-" -> 1
        | "*" -> 2
        | "/" -> 2
        | "%" -> 2
        | "^" -> 3
        |  _  -> 0   
    
    let mutable out = []
    let mutable temp = ""
    let size = in_string.Length
    for i = 0 to size - 1 do
        let v = in_string.[i] 
        if System.Char.IsDigit(v) then temp <- temp + v.ToString()
        else
            match v with
            | ' ' ->
                 if System.Char.IsDigit(in_string.[i - 1]) then
                     out <- List.append out [temp;]
                     temp <- ""
            | '-' ->
                if System.Char.IsDigit(in_string.[i + 1]) then temp <- "-"
                else out <- List.append out ["-";]
            | '(' -> out <- List.append out ["(";]
            | ')' ->
                if temp.Length > 0 then
                    out <- List.append out [temp;]
                    temp <- "" 
                out <- List.append out [")";]
            
            |  _  -> out <- List.append out [v.ToString();]
    if temp.Length > 0 then out <- List.append out [temp;]
    let stack = new Stack<string> ()
    let mutable out_string = ""
    let mutable out_array = []
    for v in out do
        if v.Length > 1 || System.Char.IsDigit(v.[0]) then 
            out_string <-  v.ToString()
            out_array <- List.append out_array [out_string;]
            out_string <- ""
            
        else
            match v with
            | "(" -> stack.push(v)
            | ")" ->
                while ((stack.head) <> "(") && (not stack.Empty) do
                    out_string <- stack.head
                    out_array <- List.append out_array [out_string;]
                    out_string <- ""
                    ignore(stack.pop)
                if ((stack.head) = "(") then ignore(stack.pop)

               
            | _   ->
                
                while not stack.Empty 
                    && (priority(stack.head) >= priority(v) && priority(v) < 3
                        || (priority(stack.head) >  priority(v) && priority(v) = 3))
                            do out_string <- stack.head
                               out_array <- List.append out_array [out_string;]
                               out_string <- ""
                               ignore(stack.pop)
                               
                               
                stack.push(v)
              
                
    while not stack.Empty do out_string <- stack.head
                             out_array <- List.append out_array [out_string;]
                             out_string <- ""
                             ignore(stack.pop)
                             
                         
    out_array
    
     
[<EntryPoint>]
let main argv =
  
  let pathRead = "MyTest.txt" 
  use stream = new StreamReader(pathRead)
  let in_string = stream.ReadLine()
  let out_string = convert (in_string)
  
  let pathWrite = "Result.txt"
  use stream2 = new StreamWriter(pathWrite)
  for i = 0 to out_string.Length - 1 do
      stream2.WriteLine (out_string.[i])
  0

