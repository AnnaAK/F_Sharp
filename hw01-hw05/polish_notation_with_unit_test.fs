(* ex. 37
   expected time: 10 hours
   real time: 4 hours
   Author: Kudryashova Anna
*)
// это файл с проверкой с помощью юнит тестов

module polish_notation_with_unit_test
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
                             
                         
    printf "%A\n" out_array
    out_array
    
[<TestFixture>]
type ``test for RPN`` () =
  
  [<Test>] 
  member this.``test1`` () =
    let in_string = "3 + 4 * 2 / (1 - 5)^2"
    Assert.AreEqual(convert (in_string), ["3"; "4"; "2"; "*"; "1"; "5"; "-"; "2"; "^"; "/"; "+"])
  [<Test>] 
  member this.``test2`` () =
    let in_string = "165 + (-165)"
    Assert.AreEqual(convert (in_string), ["165"; "-165"; "+"])
  [<Test>] 
  member this.``test3`` () =
    let in_string = "(((36))* (-26) + 1)"
    Assert.AreEqual(convert (in_string), ["36"; "-26"; "*"; "1"; "+"])
  [<Test>] 
  member this.``test4`` () =
    let in_string = "7 * (6 + 5)"
    Assert.AreEqual(convert (in_string), ["7"; "6"; "5"; "+"; "*"])
  
[<EntryPoint>]
let main argv =
  0
