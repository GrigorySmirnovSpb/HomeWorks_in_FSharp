namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open LibMergeSort

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing1 () =
        let expected = [|1; 2; 3; 4; 5; 6; 7|]
        let actual = MergeSorting.MergeSort [|5; 1; 6; 4; 3; 7; 2|] compare
        CollectionAssert.AreEqual(expected,  actual)

    [<TestMethod>]
    member this.TestMethodPassing2 () =
        let expected = [|"abcd"; "abcde"; "abcdef"|]
        let actual = MergeSorting.MergeSort [|"abcde"; "abcd"; "abcdef"|] compare
        CollectionAssert.AreEqual(expected,  actual)

    [<TestMethod>]
    member this.TestMethodPassing3() =
        let expected = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual = MergeSorting.MergeSort[|3; 4; 1; 8; 5; 2; 7; 6 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing4() =
        let expected = [| -12.7; -4.4; 0; 4.2; 7; 10 |]
        let actual:float array = MergeSorting.MergeSort [| 0; 10; -4.4; 4.2; 7; -12.7|] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing5() =
        let expected = [| 1 |]
        let actual = MergeSorting.MergeSort [| 1 |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing6() =
        let expected = [| "a"; "b"; "d"; "f"; "g"; "y"; |]
        let actual = MergeSorting.MergeSort [| "g"; "a"; "f"; "d"; "b"; "y"; |] compare
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing7() =
        let expected = [||]
        let actual = MergeSorting.MergeSort [||] compare
        CollectionAssert.AreEqual(expected, actual)
