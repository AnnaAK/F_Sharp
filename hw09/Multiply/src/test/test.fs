module test
open Program
open System.Threading
open System.Diagnostics
open NUnit.Framework

[<Test>] // вектор на вектор один поток
let ``multiply of vector and vector 1 thread ``() =
    let array1 = array2D [ [ 1]; [5]; [1] ]
    let array2 = array2D [ [ 2; 3; 4]]
    let res = multi 1 array2 array1
    Assert.AreEqual (res, [21]) 

[<Test>] // матрицу на матрицу один поток
let ``multiply of matrix and matrix 1 thread ``() =
    let array1 = array2D [ [ 1; 2; 3;]; [5; 4; 3]; [1; 1; 0] ]
    let array2 = array2D [ [ 2; 3; 4]; [-5; 0; 3]; [1; -1; 0]]
    let res = multi 1 array1 array2
    Assert.AreEqual (res, [-5; 0; 10; -7; 12; 32; -3; 3; 7]) 

[<Test>] // матрицу на матрицу три потока
let ``multiply of matrix and matrix 3 thread ``() =
    let array1 = array2D [ [ 1; 2; 3;]; [5; 4; 3]; [1; 1; 0] ]
    let array2 = array2D [ [ 2; 3; 4]; [-5; 0; 3]; [1; -1; 0]]
    let res = multi 3 array1 array2
    Assert.AreEqual (res, [-5; 0; 10; -7; 12; 32; -3; 3; 7]) 

[<Test>] // матрицу на вектор один поток
let ``multiply of matrix and vector 1 thread ``() =
    let array1 = array2D [ [ 1; 2]; [5; 4] ]
    let array2 = array2D [ [ 2]; [-5]]
    let res = multi 1 array1 array2
    Assert.AreEqual (res, [-8; -10]) 

[<Test>] // матрицу на вектор два потока
let ``multiply of matrix and vector 2 thread ``() =
    let array1 = array2D [ [ 1; 2]; [5; 4] ]
    let array2 = array2D [ [ 2]; [-5]]
    let res = multi 2 array1 array2
    Assert.AreEqual (res, [-8; -10]) 
