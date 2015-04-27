module other_radix

open System

let to10FromOther (str: string)(bas: int) =
  let  n = Convert.ToInt32(str, bas)
  n


let convertToBase (in_str: string)(bas: int) =
  let  mutable n = Convert.ToInt32(in_str, 10)
  let mutable out = ""
  let mutable number = [||]
  while (n  >= bas) do 
    number <- Array.append number [|n % bas;|]
    n <- n / bas
  number <- Array.append number [|n;|]
  let arr = Array.rev number
  for i = 0 to arr.Length - 1 do
    out <- out + arr.[i].ToString()
  let res = Convert.ToInt32(out)
  res

let convertTo_16 (in_str:string) =
  let  mutable n = Convert.ToInt32(in_str, 10)
  let mutable out = ""
  let mutable number = []
  while (n  >= 16) do 
    if (n % 16) = 10 then number <- List.append number ["A";]
                          n <- n / 16
    elif (n % 16) = 11 then number <- List.append number ["B";]
                            n <- n / 16
    elif (n % 16) = 12 then number <- List.append number ["C";]
                            n <- n / 16
    elif (n % 16) = 13 then number <- List.append number ["D";]
                            n <- n / 16
    elif (n % 16) = 14 then number <- List.append number ["E";]
                            n <- n / 16
    elif (n % 16) = 15 then number <- List.append number ["F";]
                            n <- n / 16
    else number <- List.append number [(n % 16).ToString();]
         n <- n / 16
  match n with
  | 10 -> number <- List.append number ["A";]
  | 11 -> number <- List.append number ["B";]
  | 12 -> number <- List.append number ["C";]
  | 13 -> number <- List.append number ["D";]
  | 14 -> number <- List.append number ["E";]
  | 15 -> number <- List.append number ["F";]
  | _  -> number <- List.append number [n.ToString();]
  let list = List.rev number
  for i = 0 to list.Length - 1 do
    out <- out + list.[i]
  out



  
    

    


