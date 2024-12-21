namespace TestHw1

open Microsoft.VisualStudio.TestTools.UnitTesting
open HW1

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestFibonacci1() =
        let expected = 0
        let actual = Fib.getnumber 0
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFibonacci2() =
        let expected = 1
        let actual = Fib.getnumber 1
        Assert.AreEqual(expected, actual)


    [<TestMethod>]
    member this.TestFibonacci3() =
        let expected = 1
        let actual = Fib.getnumber 2
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFibonacci4() =
        let expected = 4181
        let actual = Fib.getnumber 19
        Assert.AreEqual(expected, actual)


    [<TestMethod>]
    member this.TestFibonacci5() =
        let expected = 121393
        let actual = Fib.getnumber 26
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFibonacci6() =
        let expected = 0
        let actual = Fib.getnumber -1
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFactorial1() =
        let expected = 3628800u
        let actual = Factorial.Factorial 10u
        Assert.AreEqual(expected, actual)

    member this.TestFactorial2() =
        let expected = 120u
        let actual = Factorial.Factorial 5u
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFactorial3() =
        let expected = 1u
        let actual = Factorial.Factorial 1u
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFactorial4() =
        let expected = 1u
        let actual = Factorial.Factorial 0u
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort1() =
        let expected: int[] = [| -12; -4; 0; 4; 7; 10 |]
        let actual: int[] = Bubblesort.Bubblesort [| 0; 10; -4; 4; 7; -12 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort2() =
        let expected: int[] = [| 1 |]
        let actual: int[] = Bubblesort.Bubblesort [| 1 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort3() =
        let expected: int[] = [| -10; -10; -6; 0; 0 |]
        let actual: int[] = Bubblesort.Bubblesort [| 0; -10; -6; 0; -10 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort4() =
        let expected: int[] = [||]
        let actual: int[] = Bubblesort.Bubblesort [||]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort5() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = Bubblesort.Bubblesort [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort6() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = Bubblesort.Bubblesort [| 8; 7; 6; 5; 4; 3; 2; 1 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort7() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = Bubblesort.Bubblesort [| 3; 4; 1; 8; 5; 2; 7; 6 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort1() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = Quicksort.QuickSort [| 3; 4; 1; 8; 5; 2; 7; 6 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort2() =
        let expected: int[] = [| -12; -4; 0; 4; 7; 10 |]
        let actual: int[] = Quicksort.QuickSort [| 0; 10; -4; 4; 7; -12 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort3() =
        let expected: int[] = [| 1 |]
        let actual: int[] = Quicksort.QuickSort [| 1 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort4() =
        let expected: int[] = [| -10; -10; -6; 0; 0 |]
        let actual: int[] = Quicksort.QuickSort [| 0; -10; -6; 0; -10 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort5() =
        let expected: int[] = [||]
        let actual: int[] = Quicksort.QuickSort [||]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort6() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = Quicksort.QuickSort [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        CollectionAssert.AreEqual(expected, actual)
