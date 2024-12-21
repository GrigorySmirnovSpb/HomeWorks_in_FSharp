namespace tests

open System
open Xunit
open FsCheck
open FSharpPlus
open FsCheck.Xunit
open FSharpPlus.Data
open TreeLib

module PropertyTests =

    let areAlmostEqual (a: float32) (b: float32) (epsilon: float32) =
        if Single.IsFinite(a) && Single.IsFinite(b) then
            abs (a - b) < epsilon
        elif Single.IsNaN(a) && Single.IsNaN(b) then
            true
        elif Single.IsInfinity(a) && Single.IsInfinity(b) then
            true
        elif Single.IsNegativeInfinity(a) && Single.IsNegativeInfinity(b) then
            true
        elif Single.IsPositiveInfinity(a) && Single.IsPositiveInfinity(b) then
            true
        else
            false
    type heightTest() =

        [<Fact>]
        let heightTest() =
            let leaf1 = MyTree.createLeaf 1
            let leaf2 = MyTree.createLeaf 2
            let leaf3 = MyTree.createLeaf 3

            let tree1 = MyTree.createTree (NonEmptyList.ofList [leaf2])
            let node1 = MyTree.createTree (NonEmptyList.ofList [leaf1; tree1])
            let node2 = MyTree.createTree (NonEmptyList.ofList [leaf3])

            let tree = MyTree.createTree (NonEmptyList.ofList [node1; node2])
            Assert.Equal(3, MyTree.height tree)
        
    [<Properties(MaxTest = 100)>]
    type idTests() =

        [<Property>]
        member _.intTest(tree: MyTree<int>) = tree = MyTree.map id tree

        [<Property>]
        member _.charTest(tree: MyTree<char>) = tree = MyTree.map id tree

        [<Property>]
        member _.floatTest(tree: MyTree<float32>) =
            let treeList = MyTree.treeToList (MyTree.map id tree)
            let list = MyTree.treeToList tree
            Assert.True(List.forall2 (fun a b -> areAlmostEqual a b 1e-10f) list treeList)

        [<Property>]
        member _.arrTest(tree: MyTree<array<int>>) = tree = MyTree.map id tree

    type sumTests() =

        [<Property>]
        member _.intFoldTest(tree: MyTree<int>) =
            let sum acc value = value + acc
            let actsum = MyTree.fold sum 0 tree
            let expsum = List.sum (MyTree.treeToList tree)
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.charFoldTest(tree: MyTree<char>) =
            let sum acc value = int value + int acc
            let actsum = MyTree.fold sum 0 tree
            let expsum = int (List.sum (MyTree.treeToList tree))
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.arrFoldTest(tree: MyTree<array<int>>) =
            let sum acc value = Array.sum value + acc
            let actsum = MyTree.fold sum 0 tree
            let expsum = MyTree.treeToList tree |> List.collect Array.toList |> List.sum
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.floatFoldTest(tree: MyTree<float32>) =
            let sum acc value = value + acc
            let actsum = MyTree.fold sum 0f tree
            let expsum = MyTree.treeToList tree |> List.sum
            Assert.True(areAlmostEqual actsum expsum 1e-10f)

        [<Property>]
        member _.intFoldBackTest(tree: MyTree<int>) =
            let sum acc value = value + acc
            let actsum = MyTree.foldBack sum 0 tree
            let expsum = List.sum (MyTree.treeToList tree)
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.charFoldBackTest(tree: MyTree<char>) =
            let sum acc value = int value + int acc
            let actsum = MyTree.foldBack sum 0 tree
            let expsum = int (List.sum (MyTree.treeToList tree))
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.arrFoldBackTest(tree: MyTree<array<int>>) =
            let sum acc value = Array.sum value + acc
            let actsum = MyTree.foldBack sum 0 tree
            let expsum = MyTree.treeToList tree |> List.collect Array.toList |> List.sum
            Assert.Equal(actsum, expsum)

    type ProdTest() =

        [<Property>]
        member _.intFoldTest(tree: MyTree<int>) =
            let prod acc value = value * acc
            let actprod = MyTree.fold prod 1 tree
            let expprod = List.fold prod 1 (MyTree.treeToList tree)
            Assert.Equal(actprod, expprod)

        [<Property>]
        member _.floatFoldTest(tree: MyTree<float32>) =
            let prod acc value = value * acc
            let actprod = MyTree.fold prod 1f tree
            let expprod = MyTree.treeToList tree |> List.fold prod 1f
            Assert.True(areAlmostEqual actprod expprod 1e-10f)
