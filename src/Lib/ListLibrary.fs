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

        member this.Insert (ind: int) (el: 'elem) =

            let rec insHelper curind curlst =
                match curlst with
                | Empty -> Cons(el, Empty)
                | Cons(hd, tl) when (ind = curind) -> Cons(el, insHelper (curind + 1) tl)
                | Cons(hd, tl) -> Cons(hd, insHelper (curind + 1) tl)

            insHelper 0 this

        member this.Delete (ind: int) =

            let rec delHelper curind curlst =
                match curlst with                       
                | Empty -> Empty
                | Cons(hd,tl) when (ind = curind) -> tl
                | Cons(hd,tl) -> Cons(hd, delHelper (curind + 1) tl)

            delHelper 0 this

        member this.QuickSort(compare) : MyList<'elem> =
            let rec sortHelper (arr: MyList<'elem>) (l: int) (r: int) =

                let pivot: 'elem = arr.Item((l + r) / 2)
                let mutable i: int = l
                let mutable j: int = r
                let mutable arr1: MyList<'elem> = arr

                while i <= j do

                    while compare (arr1.Item(i)) pivot < 0 do
                        i <- i + 1

                    while compare (arr1.Item(j)) pivot > 0 do
                        j <- j - 1

                    if i <= j then
                        let c: 'elem = arr1.Item(i)
                        let a: 'elem = arr1.Item(j)
                        arr1 <- arr1.Delete i
                        arr1 <- arr1.Insert i a
                        arr1 <- arr1.Delete j
                        arr1 <- arr1.Insert j c
                        i <- i + 1
                        j <- j - 1

                    if l < j then
                        arr1 <- sortHelper arr1 l j

                    if r > i then
                        arr1 <- sortHelper arr1 i r

                arr1
            if this.Length = 0
            then Empty
            else if this.Length = 1
                then this
                else sortHelper this 0 (this.Length - 1)

        member this.Bubblesort compare =
            let sortHelper (arr1: MyList<'elem>) =

                let mutable arr = arr1
                let mutable f: int = 1
                let mutable i: int = 0

                while (f = 1) do
                    let j: int = 0
                    f <- 0

                    for j in 0 .. arr.Length - 2 - i do
                        if compare (arr.Item(j)) (arr.Item(j + 1)) > 0 then
                            let c: 'elem = arr.Item(j + 1)
                            arr <- arr.Insert (j + 1) (arr.Item(j))
                            arr <- arr.Insert j c
                            f <- 1

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
                    arr1 <- arr1.Insert 0 c
                    arr1
                else
                    let arr2 = sortHelper arr1 l ((r+l) / 2)
                    let arr3 = sortHelper arr1 ((r+l)/ 2 + 1) (r)
                    let mutable arr4 = Empty
                    let mutable j: int = 0
                    let mutable i: int = 0

                    while (i < arr2.Length) && (j < arr3.Length) do
                        if compare (arr2.Item(i)) (arr3.Item(j)) < 0 
                        then
                            arr4 <- arr4.Insert arr4.Length (arr2.Item(i))
                            i <- i + 1
                        else
                            arr4 <- arr4.Insert arr4.Length (arr3.Item(j))
                            j <- j + 1

                    if i >= arr2.Length then
                        for k in 0 .. (arr3.Length-j-1) do
                            arr4 <- arr4.Insert arr4.Length (arr3.Item(j+k))
                    else
                        for k in 0 .. (arr2.Length-i-1) do
                            arr4 <- arr4.Insert arr4.Length (arr2.Item(i+k))

                    arr4
            if this.Length = 0
            then Empty
            else sortHelper this 0 (this.Length - 1)

    let rec fromList lst =
        match lst with
        | [] -> Empty
        | hd :: tl -> Cons(hd, fromList tl)

    let rec toList lst =
        match lst with
        | Empty -> []
        | Cons(hd, tl) -> hd :: toList tl