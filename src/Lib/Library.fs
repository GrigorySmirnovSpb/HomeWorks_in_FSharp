namespace MergeSort

module MergeSort =

    let rec MergeSort (arr: 'a array) (compare):'a array = 
        if Array.length arr = 1
        then
            arr
        else 
            let arr1 = MergeSort (Array.sub arr 0 ((Array.length arr)/2)) compare
            let arr2 = MergeSort (Array.sub arr  (Array.length arr/2) ((Array.length arr)/2 + (Array.length arr)%2)) compare
            let mutable arr3: 'a array =[||]
            let mutable j: int = 0
            let mutable i: int = 0
            while (i < Array.length arr1) && (j < Array.length arr2) do
                if compare arr1.[i] arr2.[j] < 0
                then
                    arr3 <- Array.concat [arr3; [|arr1.[i]|]]
                    i <- i + 1
                else 
                    arr3 <- Array.concat [arr3; [|arr2.[j]|]]
                    j <- j + 1
            if i >= Array.length arr1
            then arr3 <- Array.concat [arr3; Array.sub arr2 j (Array.length arr2 - j)]
            else arr3 <- Array.concat [arr3; Array.sub arr1 i (Array.length arr1 - i)]
            arr3
