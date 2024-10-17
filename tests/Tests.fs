namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.Test1() =
        let expected = [ 1; 2; 3; 4; 5 ]
        let mutable lst = Lists.MyList.fromList [ 4; 5; 2; 1; 3 ]
        let lst2 = lst.MergeSort compare
        let actual = Lists.MyList.toList lst2
        Assert.AreEqual(expected, actual)
