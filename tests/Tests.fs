namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing1() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = BubbleLib.Bubble.Bubblesort [| 3; 4; 1; 8; 5; 2; 7; 6 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing2() =
        let expected: int[] = [| -12; -4; 0; 4; 7; 10 |]
        let actual: int[] = BubbleLib.Bubble.Bubblesort [| 0; 10; -4; 4; 7; -12 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing3() =
        let expected: int[] = [| 1 |]
        let actual: int[] = BubbleLib.Bubble.Bubblesort [| 1 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing4() =
        let expected: int[] = [| -10; -10; -6; 0; 0 |]
        let actual: int[] = BubbleLib.Bubble.Bubblesort [| 0; -10; -6; 0; -10 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing5() =
        let expected = 13
        let actual = Fib.Fib.getnumber 8
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing6() =
        let expected = 317811
        let actual = Fib.Fib.getnumber 29
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing7() =
        let expected = 4181
        let actual = Fib.Fib.getnumber 20
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing8() =
        let expected = 1
        let actual = Fib.Fib.getnumber 2
        Assert.AreEqual(expected, actual)
