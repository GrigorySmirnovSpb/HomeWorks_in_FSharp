namespace LibMergeSort

module MergeSorting =

    let rec MergeSort (arr: 'a array) compare : 'a array =
        if Array.length arr <= 1 then
            arr
        else
            let arr1 = MergeSort (Array.sub arr 0 ((Array.length arr) / 2)) compare

            let arr2 =
                MergeSort
                    (Array.sub arr (Array.length arr / 2) ((Array.length arr) / 2 + (Array.length arr) % 2))
                    compare

            let mutable arr3: 'a array = arr
            let mutable j: int = 0
            let mutable i: int = 0
            let mutable k: int = 0

            while (i < Array.length arr1) && (j < Array.length arr2) do
                if compare arr1.[i] arr2.[j] < 0 then
                    arr3.[k] <- arr1.[i]
                    i <- i + 1
                    k <- k + 1
                else
                    arr3.[k] <- arr2.[j]
                    j <- j + 1
                    k <- k + 1

            if i >= Array.length arr1 then
                for l in j .. (Array.length arr2 - 1) do
                    arr3.[k] <- arr2.[l]
                    k <- k + 1
            else
                for l in i .. (Array.length arr1 - 1) do
                    arr3.[k] <- arr1.[l]
                    k <- k + 1

            arr3

namespace BubbleLib

module Bubblesorting =

    let Bubblesort (arr1: 'a array) compare =
        let mutable f: int = 1
        let mutable i: int = 0

        while (f = 1) do
            let j: int = 0
            f <- 0

            for j in 0 .. Array.length arr1 - 2 - i do
                if (compare arr1[j] arr1[j + 1] > 0) then
                    let temp = arr1.[j]
                    arr1.[j] <- arr1.[j + 1]
                    arr1.[j + 1] <- temp
                    f <- 1

            i <- i + 1

        arr1

namespace QuickLib

module Quicksorting =
    let QuickSort (arr1: 'a array) compare =
        let rec QuickHelper (arr: 'a array) (l: int) (r: int) =
            let pivot = arr.[(l + r) / 2]
            let mutable i: int = l
            let mutable j: int = r

            while i <= j do
                while compare arr.[i] pivot < 0 do
                    i <- i + 1

                while compare arr.[j] pivot > 0 do
                    j <- j - 1

                if i <= j then
                    let c = arr.[i]
                    arr.[i] <- arr.[j]
                    arr.[j] <- c
                    i <- i + 1
                    j <- j - 1

            if l < j then
                QuickHelper arr l j

            if r > i then
                QuickHelper arr i r

        if arr1.Length > 0 then
            QuickHelper arr1 0 (arr1.Length - 1)
            arr1
        else
            [||]
