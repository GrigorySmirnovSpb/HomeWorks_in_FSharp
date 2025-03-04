﻿namespace LibSorting

module Sorts =

    let rec MergeSort (arr: 'a array) compare : 'a array =
        if Array.length arr <= 1 then
            arr
        else
            let length = (Array.length arr)
            let remainder = (Array.length arr) % 2
            let arr1 = MergeSort (Array.sub arr 0 (length / 2)) compare

            let arr2 = MergeSort (Array.sub arr (length / 2) (length / 2 + remainder)) compare

            let mutable arr3: 'a array = arr
            let mutable j: int = 0
            let mutable i: int = 0
            let mutable k: int = 0
            let len1 = Array.length arr1
            let len2 = Array.length arr2

            while (i < len1) && (j < len2) do
                if compare arr1.[i] arr2.[j] < 0 then
                    arr3.[k] <- arr1.[i]
                    i <- i + 1
                    k <- k + 1
                else
                    arr3.[k] <- arr2.[j]
                    j <- j + 1
                    k <- k + 1

            if i >= len1 then
                for l in j .. (len2 - 1) do
                    arr3.[k] <- arr2.[l]
                    k <- k + 1
            else
                for l in i .. (len1 - 1) do
                    arr3.[k] <- arr1.[l]
                    k <- k + 1

            arr3

    let Bubblesort (arr1: 'a array) compare =
        let mutable f: bool = true
        let mutable i: int = 0

        while f do
            let j: int = 0
            f <- false

            for j in 0 .. Array.length arr1 - 2 - i do
                if (compare arr1[j] arr1[j + 1] > 0) then
                    let temp = arr1.[j]
                    arr1.[j] <- arr1.[j + 1]
                    arr1.[j + 1] <- temp
                    f <- true

            i <- i + 1

        arr1

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
