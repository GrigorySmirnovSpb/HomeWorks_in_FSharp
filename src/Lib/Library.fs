namespace Fib

open System

module Say =
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

    let mutable ArrMatr = createMatrixArray 100 2 2
    ArrMatr.[1] <- [| [| 1; 1 |]; [| 1; 0 |] |]

    let rec powerMatrix (mat: Matrix) (p: int) : Matrix =
        if p = 1 then
            mat
        else if ArrMatr.[p].[0].[0] <> 0 then
            ArrMatr.[p]
        else
            let m1 = powerMatrix mat (p / 2)
            let m2 = powerMatrix mat (p / 2 + p % 2)
            let res = multiplyMatrix m1 m2
            ArrMatr.[p] <- res
            res

    let getnumber (n: int) : int =
        if n = 0 then
            0
        else if n = 1 then
            1
        else
            let resmat = powerMatrix q (n - 1)
            resmat.[0].[0]
