namespace tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing1() =
        let expected = 0
        let actual = FibLib.Fib.getnumber 0
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing2() =
        let expected = 1
        let actual = FibLib.Fib.getnumber 1
        Assert.AreEqual(expected, actual)


    [<TestMethod>]
    member this.TestMethodPassing3() =
        let expected = 1
        let actual = FibLib.Fib.getnumber 2
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing4() =
        let expected = 4181
        let actual = FibLib.Fib.getnumber 19
        Assert.AreEqual(expected, actual)
    

    [<TestMethod>]
    member this.TestMethodPassing5() =
        let expected = 121393
        let actual = FibLib.Fib.getnumber 26
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing6() =
        let expected = 0
        let actual = FibLib.Fib.getnumber -1
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing7() =
        let expected = 3628800
        let actual = FactorialLib.Factorial.Factorial 10
        Assert.AreEqual(expected, actual)

    member this.TestMethodPassing8() =
        let expected = 120
        let actual = FactorialLib.Factorial.Factorial 5
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing9() =
        let expected = 1
        let actual = FactorialLib.Factorial.Factorial 1
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing10() =
        let expected = 1
        let actual = FactorialLib.Factorial.Factorial 0
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing11() =
        let expected = 0
        let actual = FactorialLib.Factorial.Factorial -15
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing12() =
        let expected: int[] = [| -12; -4; 0; 4; 7; 10 |]
        let actual: int[] = BubbleLib.Bubblesort.Bubblesort [| 0; 10; -4; 4; 7; -12 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing13() =
        let expected: int[] = [| 1 |]
        let actual: int[] = BubbleLib.Bubblesort.Bubblesort [| 1 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing14() =
        let expected: int[] = [| -10; -10; -6; 0; 0 |]
        let actual: int[] = BubbleLib.Bubblesort.Bubblesort [| 0; -10; -6; 0; -10 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing15() =
        let expected: int[] = [||]
        let actual: int[] = BubbleLib.Bubblesort.Bubblesort [||]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing16() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = BubbleLib.Bubblesort.Bubblesort [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing17() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = BubbleLib.Bubblesort.Bubblesort [| 8; 7; 6; 5; 4; 3; 2; 1|]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing18() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = BubbleLib.Bubblesort.Bubblesort [| 3; 4; 1; 8; 5; 2; 7; 6 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing19() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = QuickSort.Quicksort.QuickSort [| 3; 4; 1; 8; 5; 2; 7; 6 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing20() =
        let expected: int[] = [| -12; -4; 0; 4; 7; 10 |]
        let actual: int[] = QuickSort.Quicksort.QuickSort [| 0; 10; -4; 4; 7; -12 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing21() =
        let expected: int[] = [| 1 |]
        let actual: int[] = QuickSort.Quicksort.QuickSort [| 1 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing22() =
        let expected: int[] = [| -10; -10; -6; 0; 0 |]
        let actual: int[] = QuickSort.Quicksort.QuickSort [| 0; -10; -6; 0; -10 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing23() =
        let expected: int[] = [||]
        let actual: int[] = QuickSort.Quicksort.QuickSort [||]
        CollectionAssert.AreEqual(expected, actual)
    
    [<TestMethod>]
    member this.TestMethodPassing24() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = QuickSort.Quicksort.QuickSort [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        CollectionAssert.AreEqual(expected, actual)

