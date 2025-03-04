namespace tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open QuattroMatrix
open QuattroMatrix.Matrix

module PropertyQtreeTest = 

    let toArray (mat: 'T array2d) : 'T array =
        let height = Array2D.length1 mat
        let width = Array2D.length2 mat
        Array.init (height * width) (fun i ->
            let row = i / width
            let col = i % width
            mat.[row, col]
        )

    let multMatr addFunc mulFunc mat1 mat2 = 
        if Array2D.length1 mat1 <> Array2D.length2 mat2
        then failwith "Matrices have different sizes"
        else 
            let multedMat = Array2D.zeroCreate (Array2D.length1 mat1) (Array2D.length2 mat2)
            for i in 0 .. Array2D.length1 mat1 - 1 do
                for j in 0 .. Array2D.length2 mat1 - 1 do
                    for k in 0 .. Array2D.length2 mat2 - 1 do
                    multedMat[i,j] <- addFunc multedMat[i,j] (mulFunc mat1[i,k] mat2[k,j])
            multedMat

    let createQtree matr =
        let sqmatr = createSquareMatrix matr
        let qmatr = createQuadMatrix sqmatr 0 0 (Array2D.length1 sqmatr)
        qmatr, Array2D.length1 sqmatr

    let createFinalMatrix (qtree: QuadMatrix<'T>) qsize heigth wide = 
        let qmatr2d = toArray2d qtree qsize
        let actmatr = Array2D.create heigth wide Unchecked.defaultof<'T>
        for i in 0 .. heigth - 1 do
            for j in 0 .. wide - 1 do
                actmatr[i, j] <- qmatr2d[i, j]
        actmatr

    let matrixGenerator rows cols (elementGen: Gen<'T>) : Gen<'T[,]> =
        let genMatrix =
            gen {
                let! elements = Gen.array2DOfDim (rows, cols) elementGen
                return elements
            }
        genMatrix

   [<Properties(MaxTest = 100)>]
    type idTests() =

        [<Property>]
        member _.intTest (matr: int array2d) =
            let qmatr, qsize = PropertyQtreeTest.createQtree matr
            let mapedMatr = map id qmatr
            Assert.Equal(qmatr, mapedMatr)

        [<Property>]
        member _.floatTest (matr: float32 array2d) =
            let qmatr, qsize = PropertyQtreeTest.createQtree matr
            let mapedMatr = map id qmatr
            Assert.Equal(qmatr, mapedMatr)
        
        [<Property>]
        member _.charTest (matr: char array2d) = 
            let qmatr, qsize = PropertyQtreeTest.createQtree matr
            let mapedMatr = map id qmatr
            Assert.Equal(qmatr, mapedMatr)
    
    type mapTest() =

        [<Property>]
        member _.intTest (matr: int array2d) = 
            let heigth = Array2D.length1 matr
            let wide = Array2D.length2 matr
            let expmatr = Array2D.map ((+) 1) matr
            let qmatr, qsize = PropertyQtreeTest.createQtree matr
            let mapedMatr = map ((+) 1) qmatr
            let actmatr = PropertyQtreeTest.createFinalMatrix mapedMatr qsize heigth wide
            Assert.Equal(expmatr, actmatr)

        [<Property>]
        member _.charTest (matr: char array2d) = 
            let heigth = Array2D.length1 matr
            let wide = Array2D.length2 matr
            let expmatr = Array2D.map ((+) '1') matr
            let qmatr, qsize = PropertyQtreeTest.createQtree matr
            let mapedMatr = map ((+) '1') qmatr
            let actmatr = PropertyQtreeTest.createFinalMatrix mapedMatr qsize heigth wide
            Assert.Equal(expmatr, actmatr)

        [<Property>]
        member _.floatTest (size: uint) = 
            let matr = Gen.sample (int size) 1 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {-infinityf .. infinityf}))
            let heigth = Array2D.length1 matr.[0]
            let wide = Array2D.length2 matr.[0]
            let expmatr = Array2D.map ((+) 1f) matr.[0]
            let qmatr, qsize = PropertyQtreeTest.createQtree matr.[0]
            let mapedMatr = map ((+) 1f) qmatr
            let actmatr = PropertyQtreeTest.createFinalMatrix mapedMatr qsize heigth wide
            Assert.Equal(expmatr, actmatr)

    type map2Test()=

        [<Property>]    
        member _.intTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.choose (-100000, 100000)))
            let matr11d = PropertyQtreeTest.toArray matr.[0]
            let matr21d = PropertyQtreeTest.toArray matr.[1]
            let expmatr = Array.map2 (fun x y -> x + y) matr11d matr21d    
            let qmatr1, qsize1 = PropertyQtreeTest.createQtree matr.[0]
            let qmatr2, qsize2 = PropertyQtreeTest.createQtree matr.[1]
            let actqmatr = map2 (+) qmatr1 qmatr2
            let actmatr2d = PropertyQtreeTest.createFinalMatrix actqmatr qsize1 (int size) (int size)
            let actmatr = PropertyQtreeTest.toArray actmatr2d
            let fequal = Array.forall2 (=) actmatr expmatr
            Assert.True(fequal)

        [<Property>]
        member _.charTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {' ' .. '~'}))
            let matr11d = PropertyQtreeTest.toArray matr.[0]
            let matr21d = PropertyQtreeTest.toArray matr.[1]
            let expmatr = Array.map2 (fun x y -> x + y) matr11d matr21d    
            let qmatr1, qsize1 = PropertyQtreeTest.createQtree matr.[0]
            let qmatr2, qsize2 = PropertyQtreeTest.createQtree matr.[1]
            let actqmatr = map2 (+) qmatr1 qmatr2
            let actmatr2d = PropertyQtreeTest.createFinalMatrix actqmatr qsize1 (int size) (int size)
            let actmatr = PropertyQtreeTest.toArray actmatr2d
            let fequal = Array.forall2 (=) actmatr expmatr
            Assert.True(fequal)

        [<Property>]    
        member _.floatTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {-infinityf .. infinityf}))
            let matr11d = PropertyQtreeTest.toArray matr.[0]
            let matr21d = PropertyQtreeTest.toArray matr.[1]
            let expmatr = Array.map2 (fun x y -> x + y) matr11d matr21d    
            let qmatr1, qsize1 = PropertyQtreeTest.createQtree matr.[0]
            let qmatr2, qsize2 = PropertyQtreeTest.createQtree matr.[1]
            let actqmatr = map2 (+) qmatr1 qmatr2
            let actmatr2d = PropertyQtreeTest.createFinalMatrix actqmatr qsize1 (int size) (int size)
            let actmatr = PropertyQtreeTest.toArray actmatr2d
            let fequal = Array.forall2 (=) actmatr expmatr
            Assert.True(fequal)

    type multTest() =

        [<Property>]    
        member _.intTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.choose (-100000, 100000)))
            let expmatr= PropertyQtreeTest.multMatr (+) ( * ) matr.[0] matr.[1]
            let qmatr1, qsize1 = PropertyQtreeTest.createQtree matr.[0]
            let qmatr2, qsize2 = PropertyQtreeTest.createQtree matr.[1]
            let actqmatr = multiplyMatrix ( * ) (+) qmatr1 qsize1 qmatr2 qsize2
            let actmatr = PropertyQtreeTest.createFinalMatrix actqmatr qsize1 (int size) (int size)
            Assert.Equal(expmatr, actmatr)
        
        [<Property>]    
        member _.charTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {' ' .. '~'}))
            let expmatr= PropertyQtreeTest.multMatr (+) (fun x y -> char (int x * int y)) matr.[0] matr.[1]
            let qmatr1, qsize1 = PropertyQtreeTest.createQtree matr.[0]
            let qmatr2, qsize2 = PropertyQtreeTest.createQtree matr.[1]
            let actqmatr = multiplyMatrix (fun x y -> char (int x * int y)) (+) qmatr1 qsize1 qmatr2 qsize2
            let actmatr = PropertyQtreeTest.createFinalMatrix actqmatr qsize1 (int size) (int size)
            Assert.Equal(expmatr, actmatr)

        [<Property>]    
        member _.floatTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {-infinityf .. infinityf}))
            let expmatr= PropertyQtreeTest.multMatr (+) ( * ) matr.[0] matr.[1]
            let qmatr1, qsize1 = PropertyQtreeTest.createQtree matr.[0]
            let qmatr2, qsize2 = PropertyQtreeTest.createQtree matr.[1]
            let actqmatr = multiplyMatrix ( * ) (+) qmatr1 qsize1 qmatr2 qsize2
            let actmatr = PropertyQtreeTest.createFinalMatrix actqmatr qsize1 (int size) (int size)
            Assert.Equal(expmatr, actmatr)