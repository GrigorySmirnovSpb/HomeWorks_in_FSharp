namespace tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Lists

module UnitTests =

    [<Fact>]
    let MergeTest () =
        let expected = List.sort []
        let mutable lst = MyList.fromList []
        let lst2 = lst.MergeSort compare
        let actual = MyList.toList lst2
        Assert.Equal<int>(expected, actual)


    [<Fact>]
    let BubbleTest () =
        let expected = List.sort []
        let mutable lst = MyList.fromList []
        let lst2 = lst.Bubblesort compare
        let actual = MyList.toList lst2
        Assert.Equal<int>(expected, actual)


    [<Fact>]
    let QuickTest () =
        let expected = List.sort []
        let mutable lst = MyList.fromList []
        let lst2 = lst.QuickSort compare
        let actual = MyList.toList lst2
        Assert.Equal<int>(expected, actual)


module PropertyTests =

    [<Properties(MaxTest = 1000)>]
    type MergeTest() =

        [<Property>]
        member _.intTest(testList: int list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.MergeSort compare
            let actual = MyList.toList lst2
            Assert.Equal<int>(expected, actual)

        [<Property>]
        member _.charTest(testList: char list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.MergeSort compare
            let actual = MyList.toList lst2
            Assert.Equal<char>(expected, actual)

        [<Property>]
        member _.floatTest(testList: float list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.MergeSort compare
            let actual = MyList.toList lst2
            Assert.Equal<float>(expected, actual)

    type QuickTest() =

        [<Property>]
        member _.``intTest``(testList: int list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.QuickSort compare
            let actual = MyList.toList lst2
            Assert.Equal<int>(expected, actual)

        [<Property>]
        member _.charTest(testList: char list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.QuickSort compare
            let actual = MyList.toList lst2
            Assert.Equal<char>(expected, actual)

        [<Property>]
        member _.floatTest(testList: float list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.QuickSort compare
            let actual = MyList.toList lst2
            Assert.Equal<float>(expected, actual)

    type BubbleTest() =

        [<Property>]
        member _.intTest(testList: int list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.Bubblesort compare
            let actual = MyList.toList lst2
            Assert.Equal<int>(expected, actual)

        [<Property>]
        member _.charTest(testList: char list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.Bubblesort compare
            let actual = MyList.toList lst2
            Assert.Equal<char>(expected, actual)

        [<Property>]
        member _.floatTest(testList: float list) =
            let expected = List.sort testList
            let mutable lst = MyList.fromList testList
            let lst2 = lst.Bubblesort compare
            let actual = MyList.toList lst2
            Assert.Equal<float>(expected, actual)
