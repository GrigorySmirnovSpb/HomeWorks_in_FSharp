namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing() =
        let expected = 21
        let actual = Fib.Say.getnumber 8
        Assert.AreEqual(expected, actual)
