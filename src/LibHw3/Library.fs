namespace ListLib

module MyList =

    type MyList<'elem> =
        | Empty
        | Cons of 'elem * MyList<'elem>

    let rec toList myList =
        match myList with
        | Empty -> []
        | Cons (head, tail) -> head :: toList tail

    let rec fromList lst =
        match lst with
        | [] -> Empty
        | hd :: tl -> Cons(hd, fromList tl)

    // Сортировка слиянием
    let rec merge left right compare =
        match left, right with
        | Empty, _ -> right
        | _, Empty -> left
        | Cons (x1, tail1), Cons (x2, tail2) ->
            if compare x1 x2 <= 0 then
                Cons (x1, merge tail1 right compare)
            else
                Cons (x2, merge left tail2 compare)

    let rec split list =
        match list with
        | Empty -> (Empty, Empty)
        | Cons (x, Empty) -> (Cons (x, Empty), Empty)
        | Cons (x1, Cons (x2, tail)) ->
            let left, right = split tail
            (Cons (x1, left), Cons (x2, right))

    let rec mergeSort list compare =
        match list with
        | Empty -> Empty
        | Cons (x, Empty) -> Cons (x, Empty)
        | _ ->
            let left, right = split list
            merge (mergeSort left compare) (mergeSort right compare) compare

    // Сортировка пузырьком
    let rec bubbleSort list compare =
        let rec swap list notSorted =
            match list with
            | Empty -> (Empty, notSorted)
            | Cons (x, Empty) -> (Cons (x, Empty), notSorted)
            | Cons (x1, Cons (x2, tail)) ->
                if compare x1 x2 > 0 then
                    let newTail, _ = swap (Cons (x1, tail)) true
                    (Cons (x2, newTail), true)
                else
                    let newTail, wasChanged = swap (Cons (x2, tail)) notSorted
                    (Cons (x1, newTail), wasChanged)

        let sortedList, wasChanged = swap list false
        if wasChanged then bubbleSort sortedList compare else sortedList

    // QuickSort
    let rec append list1 list2 =
        match list1 with
        | Empty -> list2
        | Cons (head, tail) -> Cons (head, append tail list2)

    let rec filter func list = 
        match list with
        | Empty -> Empty
        | Cons (elem, tl) when func elem -> Cons (elem, filter func tl)
        | Cons (elem, Empty) when func elem = false -> Empty
        | Cons (elem1, Cons(elem2, tl)) when func elem1 = false -> filter func (Cons(elem2, tl))
        | _ -> Empty

    let rec quickSort list compare =
        match list with
        | Empty -> Empty
        | Cons (pivot, tail) ->
            let less = filter (fun x -> compare x pivot < 0) tail
            let greater = filter (fun x -> compare x pivot >= 0) tail
            append (quickSort less compare) (Cons(pivot, quickSort greater compare))
