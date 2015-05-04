module Program
open System.Threading
open System.Diagnostics

let rows (a: int [,]) row =
  let mutable res = [||]
  for i in 0.. (a.GetLength 1) - 1 do
    res <- Array.append res [|a.[row, i]|]
  res

let cols (a: int [,]) col =
  let mutable res = [||]
  for i in 0.. (a.GetLength 0) - 1 do
    res <- Array.append res [|a.[i, col]|]
  res

let colsAndrows (a: int[]) (b: int []) =
  let mutable res = 0
  for i in 0..a.Length - 1 do
    res <- res + a.[i] * b.[i]
  res

let multi threadNumber (a : int[,]) (b: int[,]) =
  let res = ref (Array2D.zeroCreate (a.GetLength 0) (b.GetLength 1))
  let step = a.GetLength 0 / threadNumber
  let threadArray = Array.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          for m in (i * step)..((i+1) * step - 1) do
            for l in 0 .. (b.GetLength 1) - 1 do
              res.Value.[m,l] <- colsAndrows (rows a m) (cols b l)
        ))
    )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value
  
let diagnostic t (a: int [,]) (b: int [,]) =
  let timer = new Stopwatch()
  timer.Start()
  let returnValue = multi  t a b
  printfn "Count: %d" t
  printfn "Time: %i" timer.ElapsedMilliseconds
  returnValue 



[<EntryPoint>]
let main argv =
  let size = 10
  let a = Array2D.init 100 100 (fun i j -> 2)
  let b = Array2D.init 100 100 (fun i j -> 4)
  let res = diagnostic 50 a b
  printf "%A\n" res
  0
