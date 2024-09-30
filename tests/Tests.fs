namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing1 () =
        let expected = [|1; 2; 3; 4; 5; 6; 7|]
        let actual = MergeSort.MergeSort.MergeSort [|5; 1; 6; 4; 3; 7; 2|] compare
        CollectionAssert.AreEqual(expected,  actual)

    [<TestMethod>]
    member this.TestMethodPassing2 () =
        let expected = [|"abcd"; "abcde"; "abcdef"|]
        let actual = MergeSort.MergeSort.MergeSort [|"abcde"; "abcd"; "abcdef"|] compare
        CollectionAssert.AreEqual(expected,  actual)
