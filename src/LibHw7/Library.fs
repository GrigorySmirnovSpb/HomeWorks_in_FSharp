namespace QuattroMatrix

open System

module Matrix =
    type Nothing = 
        | Empty

    type QuadMatrix<'T> =
        | Leaf of 'T * int
        | Node of QuadMatrix<'T> * QuadMatrix<'T> * QuadMatrix<'T> * QuadMatrix<'T>

    let createSquareMatrix (inmatr: 'T array2d) =
        let logSize = 
            if Array2D.length2 inmatr = 0
                then 0.0
                else max (Math.Log2(Array2D.length1 inmatr)) (Math.Log2(Array2D.length2 inmatr))

        let isSizeFalse = not (logSize = float (int logSize))

        if isSizeFalse || Array2D.length1 inmatr <> Array2D.length2 inmatr || Array2D.length1 inmatr = 0 then
            let normalSize = pown 2 (int (logSize) + 1)

            let resMatr: 'T array2d =
                Array2D.create normalSize normalSize Unchecked.defaultof<'T>

            for i in 0 .. Array2D.length1 inmatr - 1 do
                for j in 0 .. Array2D.length2 inmatr - 1 do
                    resMatr[i, j] <- inmatr[i, j]

            resMatr
        else
            let resMatr = inmatr
            resMatr

    let rec createQuadMatrix (sqmatr: 'T array2d) icorner jcorner size =
        let mutable isEqualMatr = true
        let value = sqmatr[icorner, jcorner]
        let mutable i = icorner
        let mutable j = jcorner

        while i < icorner + size && isEqualMatr do
            while j < jcorner + size && isEqualMatr do
                if value <> sqmatr[i, j] then
                    isEqualMatr <- false
                j <- j + 1
            i <- i + 1
            j <- jcorner

        if isEqualMatr then
            Leaf(value, size)
        else
            let halfsize = size / 2

            Node(
                createQuadMatrix sqmatr icorner jcorner halfsize,
                createQuadMatrix sqmatr icorner (jcorner + halfsize) halfsize,
                createQuadMatrix sqmatr (icorner + halfsize) jcorner halfsize,
                createQuadMatrix sqmatr (icorner + halfsize) (jcorner + halfsize) halfsize
            )
        
    let rec map func qTree =
        match qTree with
        | Leaf(value, size) -> Leaf(func value, size)
        | Node(t1, t2, t3, t4) -> 
            Node(map func t1, map func t2, map func t3, map func t4)

    let rec map2 func qTree1 qTree2 =
        match qTree1, qTree2 with
        | Leaf(value1, size1), Leaf(value2, size2) when size1 = size2 -> 
            Leaf(func value1 value2, size1)
        | Leaf(value1, size1), Node(t2_1, t2_2, t2_3, t2_4) -> 
            Node(map2 func qTree1 t2_1, 
                map2 func qTree1 t2_2, 
                map2 func qTree1 t2_3, 
                map2 func qTree1 t2_4)
        | Node(t1_1, t1_2, t1_3, t1_4), Leaf(value2, size2) -> 
            Node(map2 func t1_1 qTree2, 
                map2 func t1_2 qTree2, 
                map2 func t1_3 qTree2, 
                map2 func t1_4 qTree2)
        | Node(t1_1, t1_2, t1_3, t1_4), Node(t2_1, t2_2, t2_3, t2_4) -> 
            Node(map2 func t1_1 t2_1, 
                map2 func t1_2 t2_2, 
                map2 func t1_3 t2_3, 
                map2 func t1_4 t2_4)
        | _ -> failwith "Trees must be of the same size"

    let rec add qtree1 qtree2 =
        match qtree1, qtree2 with
        | Leaf(value1, size1), Leaf(value2, size2) -> Leaf(value1 + value2, size1)
        | Node(t1_1, t1_2, t1_3, t1_4), Node(t2_1, t2_2, t2_3, t2_4) ->
            Node (add t1_1 t2_1, add t1_2 t2_2, add t1_3 t2_3, add t1_4 t2_4)
        | _ -> failwith "Trees must be of the same size"
    
    let rec multiplyMatrix qTree1 qTree2 =
        match qTree1, qTree2 with
        | Leaf(value1, size1), Leaf(value2, size2) -> Leaf(value1 * value2, size1)
        | (Node (t1_1, t1_2, t1_3, t1_4), Node (t2_1, t2_2, t2_3, t2_4)) ->
            let a = multiplyMatrix t1_1 t2_1
            let b = multiplyMatrix t1_2 t2_3
            let c = multiplyMatrix t1_1 t2_2
            let d = multiplyMatrix t1_2 t2_4
            let e = multiplyMatrix t1_3 t2_1
            let f = multiplyMatrix t1_4 t2_3
            let g = multiplyMatrix t1_3 t2_2
            let h = multiplyMatrix t1_4 t2_4
            
            Node (add a b, add c d, add e f, add g h)
        | _ -> failwith "Trees must be of the same size"

    let rec toArray2d (qTree: QuadMatrix<'T>) =
        match qTree with
        | Leaf(value, size) -> 
            let matr = Array2D.create size size value
            matr
        | Node(t1,t2, t3, t4) ->
            let topleft = toArray2d t1
            let topright = toArray2d t2
            let botleft = toArray2d t3
            let botright = toArray2d t4
            let size = Array2D.length1 topleft

            let resmatr = Array2D.create (size * 2) (size * 2) Unchecked.defaultof<'T>
            
            for i in 0 .. size - 1 do
                for j in 0 .. size - 1 do 
                    resmatr[i, j] <- topleft[i, j]
                    resmatr[i, j + size] <- topright[i, j]
                    resmatr[i + size, j] <- botleft[i, j]
                    resmatr[i + size, j + size] <- botright[i, j]
            resmatr