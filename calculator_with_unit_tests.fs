(* ex. 38 with unit tests
   Author: Kudryashova Anna

*)

open System
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


let Calculator (in_string: string) =
  let stack = new Stack<string>()
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
  let tokens = List.filter (fun x -> x <> "" ) tokens_n
 
  for i = 0 to tokens.Length - 1 do
    let v = tokens.[i]
 
    if System.Char.IsNumber(v, 0) || 
       System.Char.IsPunctuation(v, 0) && (v.Length > 1) then stack.push(v)
                                                        
                                   
                                   
    elif  (not stack.Empty) then              let a = (Convert.ToInt32(stack.head.ToString(), 10))
                                              ignore (stack.pop)
                                              if (not stack.Empty) then
                                                                      let b = (Convert.ToInt32(stack.head.ToString(), 10))
                                                                      ignore (stack.pop)
                                                                      let res = 0
                                                                      match v with
                                                                      | "+" -> stack.push((b + a).ToString())
                                                                      | "-" -> stack.push((b - a).ToString())
                                                                      | "*" -> stack.push((b * a).ToString())
                                                                      | "/" -> stack.push((b / a).ToString())
                                                                      | "%" -> stack.push((b % a).ToString())
                                                                      | "^" -> 
                                                                               let rec pow a b =
                                                                                 match b with
                                                                                 | 0 -> 1
                                                                                 | 1 -> a
                                                                                 | b -> a * (pow a (b - 1))
                                                                               if a >= 0 then stack.push((pow b a).ToString())
                                                                               else stack.push((1 / (pow b (-a))).ToString())
                                                                      | _ -> failwith "incorect operator"
                                                                      
                                                                      
 
  let  mutable out_string = ""
  while (not stack.Empty) do
    out_string <- stack.head + out_string
    ignore (stack.pop)

  out_string




[<TestCase ("10 49 +", Result = "59")>]
[<TestCase ("65 5 /", Result = "13")>]
[<TestCase ("5 65 /", Result = "0")>]
[<TestCase ("7 2 8 * +", Result = "23")>]
[<TestCase ("1 4 - 2 ^", Result = "9")>]
[<TestCase ("165 -165 +", Result = "0")>]
[<TestCase ("9999 1 + 20 10 - /", Result = "1000")>]
[<TestCase ("3 4 2 * 1 5 - 2 ^ / +", Result = "3")>]
[<TestCase ("1005 1 ^ ", Result = "1005")>]
[<TestCase ("2 -3 ^", Result = "0")>]
[<TestCase ("3 1 2 ^ ^", Result = "3")>]
[<TestCase ("1 2 - 3 -", Result = "-4")>]
let ``Tests for calculator`` in_string =
    Calculator in_string 



[<EntryPoint>]
let main argv =
  0
