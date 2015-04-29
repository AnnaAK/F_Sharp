module Max_el
open System.Threading
open System.Diagnostics

let max_el (ls : int list) l r : int =
  let mutable res = ls.[0]
  for i in l .. r do
    if ls.[i] > res then res <- ls.[i]
    printf "%A\n" ls.[i]
  res

let max_t  (list : int list) threadNumber  =
  let lst = list
  let size = lst.Length 
  let res = ref -2147483648
  let step = size / threadNumber
  let threadList = List.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          let threadRes = max_el lst (i * step) ((i+1) * step - 1)
          if threadRes > res.Value then res:= threadRes
        ))
    )
  for t in threadList do
    t.Start()
  for t in threadList do
    t.Join()
  
  res.Value

let diagnostic t l =
  let timer = new Stopwatch()
  timer.Start()
  let returnValue = max_t l t
  printfn "Count: %d" t
  printfn "Time: %i" timer.ElapsedMilliseconds
  returnValue 



