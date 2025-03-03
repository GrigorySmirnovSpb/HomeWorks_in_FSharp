namespace QuattroMatrix

open System

module Matrix =
    type QuadMatrix<'T> =
        | Leaf of 'T
        | Node of QuadMatrix<'T> * QuadMatrix<'T> * QuadMatrix<'T> * QuadMatrix<'T>

    let createSquareMatrix (inmatr: 'T array2d) =
        let logSize =
            if Array2D.length2 inmatr = 0 then
                0.0
            else
                max (Math.Log2(Array2D.length1 inmatr)) (Math.Log2(Array2D.length2 inmatr))

        let isSizeCorrect = logSize = float (int logSize)

        if
            not isSizeCorrect
            || Array2D.length1 inmatr <> Array2D.length2 inmatr
            || Array2D.length1 inmatr = 0
        then
            let normalSize = pown 2 (int logSize + 1)

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
            Leaf(value)
        else
            let halfsize = size / 2

            Node(
                createQuadMatrix sqmatr icorner jcorner halfsize,
                createQuadMatrix sqmatr icorner (jcorner + halfsize) halfsize,
                createQuadMatrix sqmatr (icorner + halfsize) jcorner halfsize,
                createQuadMatrix sqmatr (icorner + halfsize) (jcorner + halfsize) halfsize
            )

    let rec map func qtree =
        match qtree with
        | Leaf(value) -> Leaf(func value)
        | Node(t1, t2, t3, t4) -> Node(map func t1, map func t2, map func t3, map func t4)

    let rec map2 func qtree1 qtree2 =
        match qtree1, qtree2 with
        | Leaf(value1), Leaf(value2) -> Leaf(func value1 value2)
        | Leaf(value1), Node(t2_1, t2_2, t2_3, t2_4) ->
            Node(map2 func qtree1 t2_1, map2 func qtree1 t2_2, map2 func qtree1 t2_3, map2 func qtree1 t2_4)
        | Node(t1_1, t1_2, t1_3, t1_4), Leaf(value2) ->
            Node(map2 func t1_1 qtree2, map2 func t1_2 qtree2, map2 func t1_3 qtree2, map2 func t1_4 qtree2)
        | Node(t1_1, t1_2, t1_3, t1_4), Node(t2_1, t2_2, t2_3, t2_4) ->
            Node(map2 func t1_1 t2_1, map2 func t1_2 t2_2, map2 func t1_3 t2_3, map2 func t1_4 t2_4)

    let rec add addFunc qtree1 size1 qtree2 size2 =
        match qtree1, qtree2 with
        | Leaf(value1), Leaf(value2) -> Leaf(addFunc value1 value2)
        | Leaf(value1), Node(t2_1, t2_2, t2_3, t2_4) ->
            Node(
                add addFunc qtree1 (size1 / 2) t2_1 (size2 / 2),
                add addFunc qtree1 (size1 / 2) t2_2 (size2 / 2),
                add addFunc qtree1 (size1 / 2) t2_3 (size2 / 2),
                add addFunc qtree1 (size1 / 2) t2_4 (size2 / 2)
            )
        | Node(t1_1, t1_2, t1_3, t1_4), Leaf(value2) ->
            Node(
                add addFunc t1_1 (size1 / 2) qtree2 (size2 / 2),
                add addFunc t1_2 (size1 / 2) qtree2 (size2 / 2),
                add addFunc t1_3 (size1 / 2) qtree2 (size2 / 2),
                add addFunc t1_4 (size1 / 2) qtree2 (size2 / 2)
            )
        | Node(t1_1, t1_2, t1_3, t1_4), Node(t2_1, t2_2, t2_3, t2_4) ->
            Node(
                add addFunc t1_1 (size1 / 2) t2_1 (size2 / 2),
                add addFunc t1_2 (size1 / 2) t2_2 (size2 / 2),
                add addFunc t1_3 (size1 / 2) t2_3 (size2 / 2),
                add addFunc t1_4 (size1 / 2) t2_4 (size2 / 2)
            )

    let rec multiplyMatrix mulFunc addFunc qtree1 size1 qtree2 size2 =
        match qtree1, qtree2 with
        | Leaf(value1), Leaf(value2) -> Leaf(mulFunc value1 value2)
        | Leaf(value1), Node(t2_1, t2_2, t2_3, t2_4) ->
            let a = multiplyMatrix mulFunc addFunc qtree1 (size1 / 2) t2_1 (size2 / 2)
            let b = multiplyMatrix mulFunc addFunc qtree1 (size1 / 2) t2_3 (size2 / 2)
            let c = multiplyMatrix mulFunc addFunc qtree1 (size1 / 2) t2_2 (size2 / 2)
            let d = multiplyMatrix mulFunc addFunc qtree1 (size1 / 2) t2_4 (size2 / 2)
            let e = multiplyMatrix mulFunc addFunc qtree1 (size1 / 2) t2_1 (size2 / 2)
            let f = multiplyMatrix mulFunc addFunc qtree1 (size1 / 2) t2_3 (size2 / 2)
            let g = multiplyMatrix mulFunc addFunc qtree1 (size1 / 2) t2_2 (size2 / 2)
            let h = multiplyMatrix mulFunc addFunc qtree1 (size1 / 2) t2_4 (size2 / 2)

            Node(
                add addFunc a (size1 / 2) b (size2 / 2),
                add addFunc c (size1 / 2) d (size2 / 2),
                add addFunc e (size1 / 2) f (size2 / 2),
                add addFunc g (size1 / 2) h (size2 / 2)
            )
        | Node(t1_1, t1_2, t1_3, t1_4), Leaf(value2) ->
            let a = multiplyMatrix mulFunc addFunc t1_1 (size1 / 2) qtree2 (size2 / 2)
            let b = multiplyMatrix mulFunc addFunc t1_2 (size1 / 2) qtree2 (size2 / 2)
            let c = multiplyMatrix mulFunc addFunc t1_1 (size1 / 2) qtree2 (size2 / 2)
            let d = multiplyMatrix mulFunc addFunc t1_2 (size1 / 2) qtree2 (size2 / 2)
            let e = multiplyMatrix mulFunc addFunc t1_3 (size1 / 2) qtree2 (size2 / 2)
            let f = multiplyMatrix mulFunc addFunc t1_4 (size1 / 2) qtree2 (size2 / 2)
            let g = multiplyMatrix mulFunc addFunc t1_3 (size1 / 2) qtree2 (size2 / 2)
            let h = multiplyMatrix mulFunc addFunc t1_4 (size1 / 2) qtree2 (size2 / 2)

            Node(
                add addFunc a (size1 / 2) b (size2 / 2),
                add addFunc c (size1 / 2) d (size2 / 2),
                add addFunc e (size1 / 2) f (size2 / 2),
                add addFunc g (size1 / 2) h (size2 / 2)
            )
        | Node(t1_1, t1_2, t1_3, t1_4), Node(t2_1, t2_2, t2_3, t2_4) ->
            let a = multiplyMatrix mulFunc addFunc t1_1 (size1 / 2) t2_1 (size2 / 2)
            let b = multiplyMatrix mulFunc addFunc t1_2 (size1 / 2) t2_3 (size2 / 2)
            let c = multiplyMatrix mulFunc addFunc t1_1 (size1 / 2) t2_2 (size2 / 2)
            let d = multiplyMatrix mulFunc addFunc t1_2 (size1 / 2) t2_4 (size2 / 2)
            let e = multiplyMatrix mulFunc addFunc t1_3 (size1 / 2) t2_1 (size2 / 2)
            let f = multiplyMatrix mulFunc addFunc t1_4 (size1 / 2) t2_3 (size2 / 2)
            let g = multiplyMatrix mulFunc addFunc t1_3 (size1 / 2) t2_2 (size2 / 2)
            let h = multiplyMatrix mulFunc addFunc t1_4 (size1 / 2) t2_4 (size2 / 2)

            Node(
                add addFunc a (size1 / 2) b (size2 / 2),
                add addFunc c (size1 / 2) d (size2 / 2),
                add addFunc e (size1 / 2) f (size2 / 2),
                add addFunc g (size1 / 2) h (size2 / 2)
            )

    let rec toArray2d (qtree: QuadMatrix<'T>) sizem =
        match qtree with
        | Leaf(value) ->
            let matr = Array2D.create sizem sizem value
            matr
        | Node(t1, t2, t3, t4) ->
            let topleft = toArray2d t1 (sizem / 2)
            let topright = toArray2d t2 (sizem / 2)
            let botleft = toArray2d t3 (sizem / 2)
            let botright = toArray2d t4 (sizem / 2)
            let size = Array2D.length1 topleft

            let resmatr = Array2D.create sizem sizem Unchecked.defaultof<'T>

            for i in 0 .. size - 1 do
                for j in 0 .. size - 1 do
                    resmatr[i, j] <- topleft[i, j]
                    resmatr[i, j + size] <- topright[i, j]
                    resmatr[i + size, j] <- botleft[i, j]
                    resmatr[i + size, j + size] <- botright[i, j]

            resmatr