module Program_test
open Program
open NUnit.Framework


// test ex.40

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

// test ex.41

[<Test>] // проверяет работу fold, Bind, Return
let ``map in tree``() =
    let tree = Node(7, Node(10, Empty, Empty), Empty)
    let res = mapTree( fun x -> x * 3) tree
    Assert.AreEqual (res,  Node(21, Empty, Node(30, Empty, Empty)) )


[<Test>] // проверяет работу fold, Bind, Return
let ``concat in tree``() =
    let tree = Node(7, Node(10, Empty, Empty), Empty)
    let tree2 = Node (2, Node(8, Empty, Empty), Empty)
    let comb = concatTree tree tree2
    Assert.AreEqual (comb,  Node(7, Node(10, Node (2, Node(8, Empty, Empty), Empty), Empty), Empty))

