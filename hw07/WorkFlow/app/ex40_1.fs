module ex40_1

open System
open NUnit.Framework



  
type RingBuilder (vals: int) =
  member this.Bind (x, f) = f (x % vals)
    
  member this.Return x = if (x % vals >= 0) then (x % vals) else ( vals + x % vals)

let ring = RingBuilder(6)
 
 
 
let RingWorkFlow (x: int, y: int) =
  ring {
    let! x = x
    let! y = y
    return x - y
 }


[<Test>] // проверяет сумму по модулю 5
let ``sum in ring``() =
    let ring = RingBuilder(5)
    let RingWorkFlow (x: int, y: int) =
      ring {
        let! x = x
        let! y = y
        return x + y
      }
    let res = RingWorkFlow (6, 0)
    Assert.AreEqual (res,  1) 

[<Test>] // проверяет отрицательную разность по модулю 5
let ``sub in ring``() =
    let ring = RingBuilder(5)
    let RingWorkFlow (x: int, y: int) =
      ring {
        let! x = x
        let! y = y
        return x - y
      }
    let res = RingWorkFlow (6, 7)
    Assert.AreEqual (res,  4) 

[<Test>] // проверяет умножение по модулю 100
let ``multi in ring``() =
    let ring = RingBuilder(100)
    let RingWorkFlow (x: int, y: int) =
      ring {
        let! x = x
        let! y = y
        return x * y
      }
    let res = RingWorkFlow (20, 20)
    Assert.AreEqual (res,  0) 


   

