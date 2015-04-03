(*
ex. 38
Autor: Kudryashova Anna
*)
// работает только на однозначных числах

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
  for i = 0 to in_string.Length - 1 do
    let v = in_string.[i]
    
    if System.Char.IsDigit(v) then 
                                   stack.push(v.ToString())
                                   
                                   
    elif  (v <> ' ') && (not stack.Empty) then 
                                              let a = (Convert.ToInt32(stack.head.ToString(), 10))
                                              ignore (stack.pop)
                                              if (not stack.Empty) then
                                                                      let b = (Convert.ToInt32(stack.head.ToString(), 10))
                                                                      ignore (stack.pop)
                                                                      let res = 0
                                                                      match v with
                                                                      | '+' -> stack.push((a + b).ToString())
                                                                      | '-' -> stack.push((a - b).ToString())
                                                                      | '*' -> stack.push((a * b).ToString())
                                                                      | '/' -> stack.push((a / b).ToString())
                                                                      | '%' -> stack.push((a % b).ToString())
                                                                      | '^' -> 
                                                                               let rec pow a b =
                                                                                 match b with
                                                                                 | 0 -> 1
                                                                                 | 1 -> a
                                                                                 | b -> a * (pow a (b - 1))
                                                                               if a >= 0 then stack.push((pow b a).ToString())
                                                                               else stack.push((1 / (pow b (-a))).ToString())
                                                                      | _ -> failwith "incorect"
                                                                      
                                                                      
  
  let  mutable out_string = ""
  while (not stack.Empty) do
    out_string <- stack.head + out_string
    ignore (stack.pop)

  printf "%A\n" out_string
  out_string




[<EntryPoint>]
let main argv =
  let in_string = "10 4 2 * 1 5 - / +" 
  let out_string = Calculator(in_string)
 

  0
