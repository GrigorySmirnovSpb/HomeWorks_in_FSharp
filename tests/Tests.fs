namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing1() =
        let expected: int[] = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let actual: int[] = BubbleLib.Say.Bubblesort [| 3; 4; 1; 8; 5; 2; 7; 6 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing2() =
        let expected: int[] = [| -12; -4; 0; 4; 7; 10 |]
        let actual: int[] = BubbleLib.Say.Bubblesort [| 0; 10; -4; 4; 7; -12 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing3() =
        let expected: int[] = [| 1 |]
        let actual: int[] = BubbleLib.Say.Bubblesort [| 1 |]
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing4() =
        let expected: int[] = [| -10; -10; -6; 0; 0 |]
        let actual: int[] = BubbleLib.Say.Bubblesort [| 0; -10; -6; 0; -10 |]
        CollectionAssert.AreEqual(expected, actual)
