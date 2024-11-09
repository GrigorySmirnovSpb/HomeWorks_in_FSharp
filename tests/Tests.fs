namespace FactorialTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FactorialLib

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing() =
        let expected = 120
        let actual = FactorialLib.Factorial.Factorial 5
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing2() =
        let expected = 0
        let actual = FactorialLib.Factorial.Factorial -100
        Assert.AreEqual(expected, actual)
    
    [<TestMethod>]
    member this.TestMethodPassing3() =
        let expected = 1
        let actual = FactorialLib.Factorial.Factorial 1
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing4() =
        let expected = 1
        let actual = FactorialLib.Factorial.Factorial 0
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing5() =
        let expected = 2
        let actual = FactorialLib.Factorial.Factorial 2
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMethodPassing6() =
        let expected = 3628800
        let actual = FactorialLib.Factorial.Factorial 10
        Assert.AreEqual(expected, actual)