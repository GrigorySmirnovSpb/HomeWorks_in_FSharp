namespace tests

open System
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
    member this.TestMethodPassing6() =
        let expected = 4181
        let actual = FibLib.Fib.getnumber 19
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing4() =
        let expected = 121393
        let actual = FibLib.Fib.getnumber 26
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing5() =
        let expected = 0
        let actual = FibLib.Fib.getnumber -1
        Assert.AreEqual(expected, actual)
