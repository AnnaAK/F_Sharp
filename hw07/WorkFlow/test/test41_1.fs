module test41_1

open NUnit.Framework

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


