module Max_el
open System.Threading
open System.Diagnostics

let max_el (ls : int list) l r : int =
  let mutable res = ls.[l]
  for i in l .. r do
    if ls.[i] > res then res <- ls.[i]
  res

let max_t  (list : int list) threadNumber  =
  let lst = list
  let size = lst.Length 
  let res = ref  System.Int32.MinValue
  let step = size / threadNumber
  let mutable threadList = List.init (threadNumber - 1) (fun i ->
      new Thread(ThreadStart(fun _ ->
          Monitor.Enter(res)
          let threadRes = max_el lst (i * step) ((i+1) * step - 1)
          if threadRes > res.Value then res:= threadRes
          Monitor.Exit(res)
        ))
    )
  threadList <- List.append threadList  [new Thread(ThreadStart(fun _ ->
          Monitor.Enter(res)
          let threadRes = max_el lst ((threadNumber - 1) * step) (lst.Length - 1)
          res := max res.Value threadRes
          Monitor.Exit(res)
        )) ]
  for t in threadList do
    t.Start()
  for t in threadList do
    t.Join()
  
  res.Value

let diagnostic l t =
  let timer = new Stopwatch()
  timer.Start()
  let returnValue = max_t l t
  printfn "Count: %d" t
  printfn "Time: %i" timer.ElapsedMilliseconds
  returnValue 



[<EntryPoint>]
let main argv = 
    let n = 10000
    let rnd = System.Random(0)
    let lst = List.init n ( fun _ -> rnd.Next ())
    printfn "%d" (diagnostic lst 3 )
   
    0 