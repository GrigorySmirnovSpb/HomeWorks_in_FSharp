namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing1() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = QuickSort.Say.QuickSort [| 3; 4; 1; 8; 5; 2; 7; 6 |] 0 7
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing2() =
        let expected: int[] = [| -12; -4; 0; 4; 7; 10 |]
        let actual: int[] = QuickSort.Say.QuickSort [| 0; 10; -4; 4; 7; -12 |] 0 5
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing3() =
        let expected: int[] = [| 1 |]
        let actual: int[] = QuickSort.Say.QuickSort [| 1 |] 0 0
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing4() =
        let expected: int[] = [| -10; -10; -6; 0; 0 |]
        let actual: int[] = QuickSort.Say.QuickSort [| 0; -10; -6; 0; -10 |] 0 4
        CollectionAssert.AreEqual(expected, actual)
