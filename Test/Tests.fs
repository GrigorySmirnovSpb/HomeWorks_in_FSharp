namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open LibSorting

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMergeSort1() =
        let expected = [| 1; 2; 3; 4; 5; 6; 7 |]
        let actual = Sorts.MergeSort [| 5; 1; 6; 4; 3; 7; 2 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMergeSort2() =
        let expected = [| "abcd"; "abcde"; "abcdef" |]
        let actual = Sorts.MergeSort [| "abcde"; "abcd"; "abcdef" |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMergeSort3() =
        let expected = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual = Sorts.MergeSort [| 3; 4; 1; 8; 5; 2; 7; 6 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMergeSort4() =
        let expected = [| -12.7; -4.4; 0; 4.2; 7; 10 |]

        let actual: float array = Sorts.MergeSort [| 0; 10; -4.4; 4.2; 7; -12.7 |] compare

        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMergeSort5() =
        let expected = [| 1 |]
        let actual = Sorts.MergeSort [| 1 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMergeSort6() =
        let expected = [| "a"; "b"; "d"; "f"; "g"; "y" |]
        let actual = Sorts.MergeSort [| "g"; "a"; "f"; "d"; "b"; "y" |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMergeSort7() =
        let expected = [||]
        let actual = Sorts.MergeSort [||] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort1() =
        let expected = [| 1; 2; 3; 4; 5; 6; 7 |]
        let actual = Sorts.Bubblesort [| 5; 1; 6; 4; 3; 7; 2 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort2() =
        let expected = [| "abcd"; "abcde"; "abcdef" |]
        let actual = Sorts.Bubblesort [| "abcde"; "abcd"; "abcdef" |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort3() =
        let expected = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual = Sorts.Bubblesort [| 3; 4; 1; 8; 5; 2; 7; 6 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort4() =
        let expected = [| -12.7; -4.4; 0; 4.2; 7; 10 |]

        let actual: float array = Sorts.Bubblesort [| 0; 10; -4.4; 4.2; 7; -12.7 |] compare

        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort5() =
        let expected = [| 1 |]
        let actual = Sorts.Bubblesort [| 1 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort6() =
        let expected = [| "a"; "b"; "d"; "f"; "g"; "y" |]
        let actual = Sorts.Bubblesort [| "g"; "a"; "f"; "d"; "b"; "y" |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort7() =
        let expected = [||]
        let actual = Sorts.Bubblesort [||] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort1() =
        let expected = [| 1; 2; 3; 4; 5; 6; 7 |]
        let actual = Sorts.QuickSort [| 5; 1; 6; 4; 3; 7; 2 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort2() =
        let expected = [| "abcd"; "abcde"; "abcdef" |]
        let actual = Sorts.QuickSort [| "abcde"; "abcd"; "abcdef" |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort3() =
        let expected = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual = Sorts.QuickSort [| 3; 4; 1; 8; 5; 2; 7; 6 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort4() =
        let expected = [| -12.7; -4.4; 0; 4.2; 7; 10 |]

        let actual: float array = Sorts.QuickSort [| 0; 10; -4.4; 4.2; 7; -12.7 |] compare

        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort5() =
        let expected = [| 1 |]
        let actual = Sorts.QuickSort [| 1 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort6() =
        let expected = [| "a"; "b"; "d"; "f"; "g"; "y" |]
        let actual = Sorts.QuickSort [| "g"; "a"; "f"; "d"; "b"; "y" |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestQuickSort7() =
        let expected = [||]
        let actual = Sorts.QuickSort [||] compare
        CollectionAssert.AreEqual(expected, actual)
