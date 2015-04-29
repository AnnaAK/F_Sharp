module test_MaxEl
open Max_el
open NUnit.Framework

[<Test>] // положительные один поток
let ``positive: 1 thread ``() =
    let list = [1..100]
    let res = max_t list 1
    Assert.AreEqual (res, 100) 

[<Test>] // положительные четыре потока
let ``positive: 4 thread ``() =
    let list = [1..100]
    let res = max_t list 4
    Assert.AreEqual (res, 100) 

[<Test>] // отрицательные два потока
let ``negative: 2 thread ``() =
    let list = [-1000..-1]
    let res = max_t list 2
    Assert.AreEqual (res, -1) 

[<Test>] // отрицательные один поток
let ``negative: 1 thread ``() =
    let list = [-1000..-1]
    let res = max_t list 1
    Assert.AreEqual (res, -1) 


[<Test>] // смешанное два потока
let ``2 thread ``() =
    let list = [-1; 3; 0; 10; -100; -2222; 33; -5]
    let res = max_t list 2
    Assert.AreEqual (res, 33) 

[<Test>] // смешанное один поток
let ``1 thread ``() =
    let list = [-1; 3; 0; 10; -100; -2222; 33; -5]
    let res = max_t list 1
    Assert.AreEqual (res, 33) 




