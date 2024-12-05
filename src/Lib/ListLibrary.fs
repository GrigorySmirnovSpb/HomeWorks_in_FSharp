namespace Lists

module MyList =

    type MyList<'elem> =
        | Empty
        | Cons of 'elem * MyList<'elem>

        member this.Length =
            let rec len (lst) : int =
                match lst with
                | Empty -> 0
                | Cons(_, tl) -> 1 + len tl

            len this

        member this.Item(a: int) : 'elem =
            match this with
            | Empty -> failwith "Index out of bounds :)"
            | Cons(hd, ls) -> if (a = 0) then hd else ls.Item(a - 1)

        member this.Replace (ind: int) (el: 'elem)  =

            let rec repHelper curind curlst =
                match curlst with
                | Empty -> Cons(el, Empty)
                | Cons(hd, tl) when (ind = curind) -> Cons(el, tl)
                | Cons(hd, tl) -> Cons(hd, repHelper (curind + 1) tl)

            repHelper 0 this

        (*member this.Delete (ind: int) =

            let rec delHelper curind curlst =
                match curlst with                       
                | Empty -> Empty
                | Cons(hd, tl) when (ind = curind) -> tl
                | Cons(hd, tl) -> Cons(hd, delHelper (curind + 1) tl)

            delHelper 0 this
*)
        member this.QuickSort(compare) : MyList<'elem> =
            let rec sortHelper (arr1: MyList<'elem>) (l: int) (r: int) =
                
                //printf("%d\n") ((l + r) / 2)
                let pivot: 'elem = arr1.Item((l + r) / 2)
                let mutable i: int = l
                let mutable j: int = r
                let mutable arr = arr1

                while i < j do

                    while compare (arr.Item(i)) pivot < 0 do
                        i <- i + 1

                    while compare (arr.Item(j)) pivot > 0 do
                        j <- j - 1

                    if i <= j then
                        let c: 'elem = arr.Item(i)
                        let a: 'elem = arr.Item(j)
                        //printf("%d\n") i
                        arr <- arr.Replace i a
                        //printf("%A\n") arr
                        arr <- arr.Replace j c
                        //printf("%A\n") arr
                        i <- i + 1
                        j <- j - 1

                if l < j then
                    arr <- sortHelper arr l j

                if r > i then
                    arr <- sortHelper arr i r
                        
                arr
            if this.Length > 0
            then sortHelper this 0 (this.Length - 1)
            else Empty            

        (*member this.QuickSort2 (compare) =               
            let partition (arr1: MyList<'elem>) (low: int) (high: int) =
                let mutable arr = arr1 
                let pivot = arr.Item(high)
                let mutable i = low - 1

                for j in low .. (high - 1) do
                    if compare (arr.Item(j)) pivot <= 0 then
                        i <- i + 1
                        let c: 'elem = arr.Item(i)
                        let a: 'elem = arr.Item(j)
                        arr <- arr.Replace i a
                        arr <- arr.Replace j c


                let c: 'elem = arr.Item(i + 1)
                let a: 'elem = arr.Item(high)
                arr <- arr.Replace (i + 1) a
                arr <- arr.Replace high c
                i + 1

            let rec inplaceQuick (arr: MyList<'elem>) (low: int) (high: int) =
                
                let mutable arr1 = arr
                if low < high 
                then
                    let pivotIndex = partition arr low high
                    arr1 <- inplaceQuick arr low (pivotIndex - 1)
                    arr1 <- inplaceQuick arr (pivotIndex + 1) high
                arr1
                     

            inplaceQuick this 0 (this.Length - 1) *)

        member this.Bubblesort compare =
            let sortHelper (arr1: MyList<'elem>) =

                let mutable arr = arr1
                let mutable flag: bool = true
                let mutable i: int = 0

                while (flag) do
                    let j: int = 0
                    flag <- false

                    for j in 0 .. arr.Length - 2 - i do
                        if compare (arr.Item(j)) (arr.Item(j + 1)) > 0 then
                            let c: 'elem = arr.Item(j + 1)
                            arr <- arr.Replace (j + 1) (arr.Item(j))
                            arr <- arr.Replace j c
                            flag <- true

                    i <- i + 1

                arr
            if this.Length = 0
            then Empty
            else sortHelper this

        member this.MergeSort(compare) : MyList<'elem> =
            let rec sortHelper (arr: MyList<'elem>) (l: int) (r: int) : MyList<'elem> =
                let mutable arr1 = arr

                if l = r then
                    let c = arr1.Item(l)
                    arr1 <- Empty
                    arr1 <- arr1.Replace 0 c
                    arr1
                else
                    let arr2 = sortHelper arr1 l ((r + l) / 2)
                    let arr3 = sortHelper arr1 ((r + l)/ 2 + 1) (r)
                    let mutable arr4 = Empty
                    let mutable j: int = 0
                    let mutable i: int = 0

                    while (i < arr2.Length) && (j < arr3.Length) do
                        if compare (arr2.Item(i)) (arr3.Item(j)) < 0 
                        then
                            arr4 <- arr4.Replace arr4.Length (arr2.Item(i))
                            i <- i + 1
                        else
                            arr4 <- arr4.Replace arr4.Length (arr3.Item(j))
                            j <- j + 1 

                    if i >= arr2.Length then
                        for k in 0 .. (arr3.Length - j - 1) do
                            arr4 <- arr4.Replace arr4.Length (arr3.Item(j + k))
                    else
                        for k in 0 .. (arr2.Length - i - 1) do                  
                            arr4 <- arr4.Replace arr4.Length (arr2.Item(i + k))

                    arr4
            if this.Length = 0
            then Empty
            else sortHelper this 0 (this.Length - 1)

    let rec fromList lst =
        //printf("%A\n") lst
        match lst with
        | [] -> Empty
        | hd :: tl -> Cons(hd, fromList tl)

    let rec toList lst =
        match lst with
        | Empty -> []
        | Cons(hd, tl) -> hd :: toList tl