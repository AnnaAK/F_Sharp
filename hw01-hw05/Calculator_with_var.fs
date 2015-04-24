(* ex. 36
   Author: Kudryashova Anna

*)

open NUnit.Framework
open System

type Tree<'A> = Empty| Num of int| Op of string * Tree<'A> * Tree<'A>


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


let convert (in_string : string, var: string) =
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
            | "x" -> out_string <- var
                     out_array <- List.append out_array [out_string;]
                     out_string <- ""

               
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
                             
   
    
    let mutable out_string1 = ""
    for i = 0 to out_array.Length - 1 do
     out_string1 <- out_string1 + " "
     out_string1 <- out_string1 + out_array.[i]
    printf "RPN: %A\n"   out_string1                  
    out_string1









let Calc_tree (in_string: string) =
  let calc = new Stack<Tree<string>> ()
  let mutable number = ""
  let mutable tokens_n = []
  
  for i = 0 to in_string.Length - 1 do
    if  (in_string.[i] = '-') && (i <> (in_string.Length - 1)) then
                                   if (System.Char.IsDigit (in_string.[i + 1]))  then number <-  number + in_string.[i].ToString()
                                   else  number <-  number + in_string.[i].ToString() 
                                         tokens_n <- List.append tokens_n [number;]
                                         number <- ""
                                                                                        
    elif (in_string.[i] = '+') || (in_string.[i] = '-') || (in_string.[i] = '*') || 
         (in_string.[i] = '/') || (in_string.[i] = '%') || (in_string.[i] = '^') then
                                  number <-  number + in_string.[i].ToString() 
                                  tokens_n <- List.append tokens_n [number;]
                                  number <- ""
    elif in_string.[i] <> ' '  then
                                    number <-  number + in_string.[i].ToString() 
                                   
    else tokens_n <- List.append tokens_n [number;]
         number <- ""

  let in_string = List.filter (fun x -> x <> "" ) tokens_n

  let calc = new Stack<Tree<string>> ()
  for i = 0 to in_string.Length - 1 do
    let v = in_string.[i]
    
    if  System.Char.IsNumber(v, 0) then 
                                   calc.push(Num(Convert.ToInt32(v.ToString(), 10)))
                                   
                                   
    elif (not calc.Empty) then 
                                              let a = calc.head
                                              ignore (calc.pop)
                                              if (not calc.Empty) then
                                                                      let b = calc.head
                                                                      ignore (calc.pop)
                                                                      calc.push(Op (v.ToString(), b, a))
                              
                                                                      
  let result = calc.head
  ignore(calc.pop) 
  printf "tree: %A\n" result
  result 
  
  
let Calc (in_string: Tree<'A>) =
  
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
  
  
  
  
let calculator (in_string: string, var:string) =
  let convert = convert (in_string, var)
  let tree = Calc_tree (convert)
  let calc = Calc (tree)
  
  calc
  
  
[<TestCase ("4 + x", "2" , Result = 6)>]
[<TestCase ("7 * (8 - x)", "0", Result = 56)>]
[<TestCase ("(5 - x) / 2","1", Result = 2)>]
[<TestCase ("(1 + 1 + 1 + 1 + 1 + 1 + 1 + 2) * x", "7", Result = 63)>]
[<TestCase ("11 ^ 1 ^ x","999", Result = 11)>]
[<TestCase ("x ^ 2 + 68","2", Result = 72)>]
[<TestCase ("x + x","4", Result = 8)>]
let ``Tests for calculator`` in_string var =
    calculator (in_string, var) 
  
  
                                                                 

  

[<EntryPoint>]
let main argv =

  0
         
