namespace tests

open System
open Xunit
open FsCheck
open FSharpPlus
open FsCheck.Xunit
open FSharpPlus.Data
open TreeLib

module PropertyTreeTests =

    type heightTest() =

        [<Fact>]
        let heightTest () =
            let leaf1 = MyTree.createLeaf 1
            let leaf2 = MyTree.createLeaf 2
            let leaf3 = MyTree.createLeaf 3

            let tree1 = MyTree.createTree (NonEmptyList.ofList [ leaf2 ])
            let node1 = MyTree.createTree (NonEmptyList.ofList [ leaf1; tree1 ])
            let node2 = MyTree.createTree (NonEmptyList.ofList [ leaf3 ])

            let tree = MyTree.createTree (NonEmptyList.ofList [ node1; node2 ])
            Assert.Equal(3, MyTree.height tree)

    let areAlmostEqual (a: float32) (b: float32) (epsilon: float32) =
        if Single.IsFinite(a) && Single.IsFinite(b) then abs (a - b) < epsilon 
        elif Single.IsNaN(a) && Single.IsNaN(b) then true
        elif Single.IsInfinity(a) && Single.IsInfinity(b) then true
        elif Single.IsNegativeInfinity(a) && Single.IsNegativeInfinity(b) then true
        elif Single.IsPositiveInfinity(a) && Single.IsPositiveInfinity(b) then true
        else false

    [<Properties(MaxTest = 100)>]
    type idTests() =

        [<Property>]
        member _.intTest(tree: MyTree<int>) =
            let treem = MyTree.map id tree
            Assert.Equal(tree, treem)

        [<Property>]
        member _.charTest(tree: MyTree<char>) =
            let treem = MyTree.map id tree
            Assert.Equal(tree, treem)

        [<Property>]
        member _.floatTest(tree: MyTree<float32>) =
            let treem = MyTree.map id tree
            Assert.Equal(tree, treem)

        [<Property>]
        member _.arrTest(tree: MyTree<array<int>>) =
            let treem = MyTree.map id tree
            Assert.Equal(tree, treem)

    type MapTests() =

        [<Property>]
        member _.intTest(tree: MyTree<int>) =
            let treem = MyTree.treeToList (MyTree.map ((+) 2) tree)
            let expList = List.map ((+) 2) (MyTree.treeToList tree)
            expList = treem

        [<Property>]
        member _.charTest(tree: MyTree<char>) =
            let treem = MyTree.treeToList (MyTree.map (fun c -> char (int c + 2)) tree)
            let expList = List.map (fun c -> char (int c + 2)) (MyTree.treeToList tree)
            expList = treem

        [<Property>]
        member _.floatTest(tree: MyTree<float32>) =
            let treem = MyTree.treeToList (MyTree.map ((+) 2f) tree)
            let expList = List.map ((+) 2f) (MyTree.treeToList tree)
            Assert.True(List.forall2 (fun a b -> areAlmostEqual a b 1e-10f)expList treem)

        [<Property>]
        member _.arrTest(tree: MyTree<array<int>>) =
            let treem = MyTree.treeToList (MyTree.map (Array.map ((+) 2)) tree)
            let expList = List.map (Array.map ((+) 2)) (MyTree.treeToList tree)
            expList = treem

    type sumTests() =

        [<Property>]
        member _.intFoldTest(tree: MyTree<int>) =
            let sum acc value = value + acc
            let actsum = MyTree.fold sum 0 tree
            let expsum = int (List.sum (MyTree.treeToList tree))
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
            let expsum = List.fold (fun x acc -> sum acc x) 0f (MyTree.treeToList tree)
            Assert.Equal(actsum, expsum)

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

        [<Property>]
        member _.floatFoldBackTest(tree: MyTree<float32>) =
            let sum acc value = value + acc
            let actsum = MyTree.foldBack sum 0f tree
            let expsum = List.foldBack (fun x acc -> sum acc x) (MyTree.treeToList tree) 0f
            Assert.Equal(actsum, expsum)

    type ProdTest() =

        [<Property>]
        member _.intFoldTest(tree: MyTree<int>) =
            let prod acc value = value * acc
            let actprod = MyTree.fold prod 2 tree
            let expprod = List.fold prod 2 (MyTree.treeToList tree)
            Assert.Equal(actprod, expprod)

        [<Property>]
        member _.floatFoldTest(tree: MyTree<float32>) =
            let prod acc value = value * acc
            let actprod = MyTree.fold prod 2f tree
            let expprod = List.fold prod 2f (MyTree.treeToList tree)
            Assert.Equal(actprod, expprod)

    type AssocSumFoldsTests() =

        [<Property>]
        member _.intFoldTest(tree1, tree2, tree3: MyTree<int>) =
            let sum acc value = value + acc
            let actsum = MyTree.fold sum (MyTree.fold sum (MyTree.fold sum 0 tree1) tree2) tree3
            let expsum = MyTree.fold sum (MyTree.fold sum (MyTree.fold sum 0 tree2) tree3) tree1
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.charFoldTest(tree1, tree2, tree3: MyTree<char>) =
            let sum acc value = int value + int acc
            let actsum = MyTree.fold sum (MyTree.fold sum (MyTree.fold sum 0 tree1) tree2) tree3
            let expsum = MyTree.fold sum (MyTree.fold sum (MyTree.fold sum 0 tree2) tree3) tree1
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.arrFoldTest(tree1, tree2, tree3: MyTree<array<int>>) =
            let sum acc value = Array.sum value + acc
            let actsum = MyTree.fold sum (MyTree.fold sum (MyTree.fold sum 0 tree1) tree2) tree3
            let expsum = MyTree.fold sum (MyTree.fold sum (MyTree.fold sum 0 tree2) tree3) tree1
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.intFoldBackTest(tree1, tree2, tree3: MyTree<int>) =
            let sum acc value = value + acc
            let actsum = MyTree.foldBack sum (MyTree.foldBack sum (MyTree.foldBack sum 0 tree1) tree2) tree3
            let expsum = MyTree.foldBack sum (MyTree.foldBack sum (MyTree.foldBack sum 0 tree2) tree3) tree1
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.charFoldBackTest(tree1, tree2, tree3: MyTree<char>) =
            let sum acc value = int value + int acc
            let actsum = MyTree.foldBack sum (MyTree.foldBack sum (MyTree.foldBack sum 0 tree1) tree2) tree3
            let expsum = MyTree.foldBack sum (MyTree.foldBack sum (MyTree.foldBack sum 0 tree2) tree3) tree1
            Assert.Equal(actsum, expsum)

        [<Property>]
        member _.arrFoldBackTest(tree1, tree2, tree3: MyTree<array<int>>) =
            let sum acc value = Array.sum value + acc
            let actsum = MyTree.foldBack sum (MyTree.foldBack sum (MyTree.foldBack sum 0 tree1) tree2) tree3
            let expsum = MyTree.foldBack sum (MyTree.foldBack sum (MyTree.foldBack sum 0 tree2) tree3) tree1
            Assert.Equal(actsum, expsum)



    type AssocMapTests() =

        [<Property>]
        member _.intTest(tree: MyTree<int>) =
            let treem = MyTree.map ((+) -4) tree |> MyTree.map ((+) 3) |> MyTree.map ((+) 2)
            let treem2 = MyTree.map ((+) 2) tree |> MyTree.map ((+) -4) |> MyTree.map ((+) 3)
            Assert.Equal(treem, treem2)

        [<Property>]    
        member _.charTest(tree: MyTree<char>) =
            let treem = MyTree.map (fun c -> char (int c + -4)) tree |> MyTree.map (fun c -> char (int c + 3)) |> MyTree.map (fun c -> char (int c + 2))
            let treem2 = MyTree.map (fun c -> char (int c + 2)) tree |> MyTree.map (fun c -> char (int c + -4)) |> MyTree.map (fun c -> char (int c + 3))
            Assert.Equal(treem, treem2)

        [<Property>]
        member _.arrTest(tree: MyTree<array<int>>) =
            let treem = MyTree.map (Array.map ((+) -4)) tree |> MyTree.map (Array.map ((+) 3)) |> MyTree.map (Array.map ((+) 2))
            let treem2 = MyTree.map (Array.map ((+) 2)) tree |> MyTree.map (Array.map ((+) -4)) |> MyTree.map (Array.map ((+) 3))
            Assert.Equal(treem, treem2)