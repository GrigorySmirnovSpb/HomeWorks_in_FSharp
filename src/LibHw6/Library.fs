namespace TreeLib

open System
open FSharpPlus
open FSharpPlus.Data

type MyTree<'elem> =
    | Node of MyTree<'elem> NonEmptyList
    | Leaf of 'elem

module MyTree =
    let createLeaf value = Leaf value

    let createTree children = Node children

    let rec fold func acc tree =
        match tree with
        | Leaf value -> func value acc
        | Node children -> NonEmptyList.fold (fun acc child -> fold func acc child) acc children

    let rec foldBack func acc tree =
        match tree with
        | Leaf value -> func value acc
        | Node children -> NonEmptyList.foldBack (fun child acc -> foldBack func acc child) children acc

    let rec map func tree =
        match tree with
        | Leaf value -> Leaf(func value)
        | Node children -> Node(NonEmptyList.map (fun child -> map func child) children)

    let height tree =
        let rec heighthelper nowlen tree =
            match tree with
            | Leaf _ -> nowlen
            | Node children ->
                let childsHeigths =
                    NonEmptyList.map (fun child -> heighthelper (nowlen + 1) child) children

                NonEmptyList.fold max nowlen childsHeigths

        heighthelper 0 tree

    let rec treeToList tree =
        match tree with
        | Leaf value -> [ value ]
        | Node children -> NonEmptyList.fold (fun acc child -> acc @ treeToList child) [] children
