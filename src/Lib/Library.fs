namespace QuickSort

module Say =
    let rec QuickSort (arr1: int array) (l: int) (r: int) : int array =
        let mutable arr: int array = arr1
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
            arr <- QuickSort arr l j

        if r > i then
            arr <- QuickSort arr i r

        arr
