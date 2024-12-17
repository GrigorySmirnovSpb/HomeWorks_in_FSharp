namespace tests

open Xunit
open FsCheck
open FSharpPlus
open FsCheck.Xunit
open FSharpPlus.Data
open TreeLib

module PropertyTests =

    [<Properties(MaxTest = 100)>] 
    type idTests() =
  
        [<Property>]
        member _.intTest (tree: MyTree<int>) =
            tree = MyTree.map id tree

        [<Property>]
        member _.charTest (tree: MyTree<char>) =
            tree = MyTree.map id tree

        (*[<Property>]
        member _.floatTest (tree: MyTree<float>) =
            tree = MyTree.map id tree*)

        [<Property>]
        member _.arrTest (tree: MyTree<array<int>>) =
            tree = MyTree.map id tree

    type sumTests() =
  
        [<Property>]
        member _.intFoldTest (tree: MyTree<int>) =
            let sum acc value = value + acc
            let actsum = MyTree.fold sum 0 tree
            let expsum = List.sum (MyTree.treeToList tree)
            actsum = expsum

        [<Property>]
        member _.charFoldTest (tree: MyTree<char>) =
            let sum value acc = int value + int acc
            let actsum = MyTree.fold sum 0 tree
            let expsum = int (List.sum (MyTree.treeToList tree))
            actsum = expsum

        [<Property>]
        member _.arrFoldTest (tree: MyTree<array<int>>) =
            let sum value acc = Array.sum value + acc
            let actsum = MyTree.fold sum 0 tree
            let expsum = MyTree.treeToList tree |> List.collect Array.toList |> List.sum
            actsum = expsum

        [<Property>]
        member _.intFoldBackTest (tree: MyTree<int>) =
            let sum value acc = value + acc
            let actsum = MyTree.foldBack sum 0 tree
            let expsum = List.sum (MyTree.treeToList tree)
            actsum = expsum

        [<Property>]
        member _.charFoldBackTest (tree: MyTree<char>) =
            let sum value acc = int value + int acc
            let actsum = MyTree.foldBack sum 0 tree
            let expsum = int (List.sum (MyTree.treeToList tree))
            actsum = expsum

        [<Property>]
        member _.arrFoldBackTest (tree: MyTree<array<int>>) =
            let sum value acc = Array.sum value + acc
            let actsum = MyTree.foldBack sum 0 tree
            let expsum = MyTree.treeToList tree |> List.collect Array.toList |> List.sum
            actsum = expsum

    type ProdTest() =

        [<Property>]
        member _.intFoldTest (tree: MyTree<int>) =
            let prod value acc = value * acc
            let actsum = MyTree.fold prod 1 tree
            let expsum = List.fold prod 1 (MyTree.treeToList tree)
            actsum = expsum

        (*[<Property>]
        member _.charFoldTest (tree: MyTree<char>) =
            let prod value acc = int value * acc
            let actsum = MyTree.fold prod 1 tree
            let expsum = int (List.fold prod 1 (MyTree.treeToList tree))
            actsum = expsum*)

        (*[<Property>]
        member _.floatTest (tree: MyTree<float>) =
            tree = MyTree.map id tree*)

            


        
