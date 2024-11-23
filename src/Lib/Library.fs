﻿namespace FibLib

open System

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
