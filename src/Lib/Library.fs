namespace BubbleLib

open System

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