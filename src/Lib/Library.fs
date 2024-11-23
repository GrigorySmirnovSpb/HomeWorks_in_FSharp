namespace FibLib

module Fib =
    type Matrix = int array array

    let q: Matrix = [| [| 1; 1 |]; [| 1; 0 |] |]

    let createMatrixArray (size: int) (rows: int) (cols: int) : Matrix array =
       Array.init size (fun _ -> Array.init rows (fun _ -> Array.init cols (fun _ -> 0)))

    let multiplyMatrix (mat1: Matrix) (mat2: Matrix) : Matrix =
        let res = Array.init 2 (fun _ -> Array.create 2 0)

        for i in 0..1 do
            for j in 0..1 do
                for k in 0..1 do
                    res.[i].[j] <- res.[i].[j] + mat1.[i].[k] * mat2.[k].[j]

        res


    let rec powerMatrix (arr: array<Matrix>) (mat: Matrix) (p: int) : Matrix =
        if p = 1 then
            mat
        else if arr.[p].[0].[0] <> 0 then
            arr.[p]
        else
            let m1 = powerMatrix arr mat (p / 2)
            let m2 = powerMatrix arr mat (p / 2 + p % 2)
            let res = multiplyMatrix m1 m2
            arr.[p] <- res
            res

    let getnumber (n: int) : int =
        if n = 0 then
            0
        else if n = 1 then
            1
        else if n < 0 then
            0
        else
            let ArrMatr = createMatrixArray (n) 2 2
            ArrMatr.[1] <- [| [| 1; 1 |]; [| 1; 0 |] |]
            let resmat = powerMatrix ArrMatr q (n-1)
            resmat.[0].[0]

namespace FactorialLib

module Factorial =
    let rec Factorial x : int = 
        if x < 0 
        then 0 
        else if x = 0 
        then 1
        else if x < 2
        then x
        else Factorial (x - 1) * x

namespace BubbleLib

module Bubblesort =
    
    let Bubblesort arr1 =
        let mutable f: int = 1
        let mutable i: int = 0

        while (f = 1) do
            let j: int = 0
            f <- 0

            for j in 0 .. Array.length arr1 - 2 - i do
                if (arr1[j] > arr1[j + 1]) then
                    let temp = arr1.[j]
                    arr1.[j] <- arr1.[j+1]
                    arr1.[j+1] <- temp
                    f <- 1

            i <- i + 1

        arr1

namespace QuickSort

module Quicksort =
    let QuickSort (arr1: int array) : int array =
        let rec QuickHelper (arr: int array) (l: int) (r: int) =
            let pivot: int = arr.[(l + r) / 2]
            let mutable i: int = l
            let mutable j: int = r

            while i <= j do
                while arr.[i] < pivot do
                    i <- i + 1

                while arr.[j] > pivot do
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

        if arr1.Length > 0
        then 
            QuickHelper arr1 0 (arr1.Length-1)
            arr1
        else [||]
