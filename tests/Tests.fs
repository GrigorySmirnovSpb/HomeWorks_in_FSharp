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
        member _.intTest (tree: MyTree<int>) =
            let sum value acc = value + acc
            let actsum = MyTree.fold sum tree 0
            let expsum = List.sum (MyTree.treeToList tree)
            actsum = expsum

        [<Property>]
        member _.charTest (tree: MyTree<char>) =
            let sum value acc = int value + int acc
            let actsum = MyTree.fold sum tree 0
            let expsum = int (List.sum (MyTree.treeToList tree))
            actsum = expsum

        [<Property>]
        member _.arrTest (tree: MyTree<array<int>>) =
            let sum value acc = Array.sum value + acc
            let actsum = MyTree.fold sum tree 0
            let expsum = MyTree.treeToList tree |> List.collect Array.toList |> List.sum
            actsum = expsum

        (*[<Property>]
        member _.floatTest (tree: MyTree<float>) =
            tree = MyTree.map id tree*)

            


        
