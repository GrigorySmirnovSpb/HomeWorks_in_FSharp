namespace FacTest

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FactorLib

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing() =
        let expected = 120
        let actual = FactorLib.Factor.factor 5
        Assert.AreEqual(expected, actual)
