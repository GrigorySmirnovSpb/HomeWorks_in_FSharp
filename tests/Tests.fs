namespace FactorialTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open FactorialLib

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
    member this.TestMethodPassing7() =
        let expected = 1
        let actual = FibLib.Fib.getnumber 2
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing8() =
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

    [<TestMethod>]
    member this.TestMethodPassing6() =
        let expected = 3628800
        let actual = FactorialLib.Factorial.Factorial 10
        Assert.AreEqual(expected, actual)

    member this.TestMethodPassing() =
        let expected = 120
        let actual = FactorialLib.Factorial.Factorial 5
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing3() =
        let expected = 1
        let actual = FactorialLib.Factorial.Factorial 1
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing9() =
        let expected = 1
        let actual = FactorialLib.Factorial.Factorial 0
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing10() =
        let expected = 0
        let actual = FactorialLib.Factorial.Factorial -15
        Assert.AreEqual(expected, actual)