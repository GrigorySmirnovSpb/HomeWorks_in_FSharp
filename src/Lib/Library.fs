namespace LibMergeSort

module MergeSorting =

    let rec MergeSort (arr: 'a array) compare :'a array =
        if Array.length arr <= 1
        then
            arr
        else
            let arr1 = MergeSort (Array.sub arr 0 ((Array.length arr)/2)) compare
            let arr2 = MergeSort (Array.sub arr  (Array.length arr/2) ((Array.length arr)/2 + (Array.length arr)%2)) compare
            let mutable arr3: 'a array = arr
            let mutable j: int = 0
            let mutable i: int = 0
            let mutable k: int = 0
            while (i < Array.length arr1) && (j < Array.length arr2) do
                if compare arr1.[i] arr2.[j] < 0
                then
                    arr3.[k] <- arr1.[i]
                    i <- i + 1
                    k <- k + 1
                else
                    arr3.[k] <- arr2.[j]
                    j <- j + 1
                    k <- k + 1
            if i >= Array.length arr1
            then
                for l in j..(Array.length arr2 - 1) do
                    arr3.[k] <- arr2.[l]
                    k <- k + 1
            else
                for l in i..(Array.length arr1 - 1) do
                    arr3.[k] <- arr1.[l]
                    k <- k + 1
            arr3
