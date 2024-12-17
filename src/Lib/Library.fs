namespace FibLib

module Fib =
    type Matrix = int array array
    let q: Matrix = [| [| 1; 1 |]; [| 1; 0 |] |]

    let createMatrixArray (size: int) (rows: int) (cols: int) : Matrix array =
        Array.init size (fun _ -> Array.init rows (fun _ -> Array.init cols (fun _ -> 0)))

    let multiplyMatrix (mat1: Matrix) (mat2: Matrix) : Matrix =
        [| [| mat1.[0].[0] * mat2.[0].[0] + mat1.[0].[1] * mat2.[1].[0]
              mat1.[0].[0] * mat2.[0].[1] + mat1.[0].[1] * mat2.[1].[1] |]
           [| mat1.[1].[0] * mat2.[0].[0] + mat1.[1].[1] * mat2.[1].[0]
              mat1.[1].[0] * mat2.[0].[1] + mat1.[1].[1] * mat2.[1].[1] |] |]


    let rec powerMatrix (arr: array<Matrix>) (mat: Matrix) (p: int) : Matrix =
        if p = 1 then
            mat
        elif arr.[p].[0].[0] <> 0 then
            arr.[p]
        else
            let m1 = powerMatrix arr mat (p / 2)

            let res =
                if p % 2 = 0 then
                    multiplyMatrix m1 m1
                else
                    let m2 = multiplyMatrix m1 q
                    multiplyMatrix m1 m2

            arr.[p] <- res
            res

    let getnumber (n: int) : int =
        if n < 1 then
            0
        else
            let ArrMatr = createMatrixArray (n + 1) 2 2
            ArrMatr.[1] <- q
            ArrMatr.[0] <- q
            let resmat = powerMatrix ArrMatr q (n - 1)
            resmat.[0].[0]

namespace FactorialLib

module Factorial =
    let rec Factorial x =
        if x <= 1u then 1u else Factorial(x - 1u) * x

namespace BubbleLib

module Bubblesort =

    let Bubblesort arr1 =
        let mutable flagexit: bool = true
        let mutable i: int = 0

        while flagexit do
            let j: int = 0
            flagexit <- false

            for j in 0 .. Array.length arr1 - 2 - i do
                if (arr1[j] > arr1[j + 1]) then
                    let temp = arr1.[j]
                    arr1.[j] <- arr1.[j + 1]
                    arr1.[j + 1] <- temp
                    flagexit <- true
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

        if arr1.Length > 0 then
            QuickHelper arr1 0 (arr1.Length - 1)
            arr1
        else
            [||]
