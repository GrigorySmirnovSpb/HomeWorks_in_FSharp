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

    let multMatr mulFunc addFunc mat1 mat2 = 
        if Array2D.length1 mat1 <> Array2D.length2 mat2
        then failwith "Matrices have different sizes"
        else 
            let multedMat = Array2D.zeroCreate (Array2D.length1 mat1) (Array2D.length2 mat2)
            for i in 0 .. Array2D.length1 mat1 - 1 do
                for j in 0 .. Array2D.length2 mat1 - 1 do
                    for k in 0 .. Array2D.length2 mat2 - 1 do
                    multedMat[i,j] <- addFunc multedMat[i,j] (mulFunc mat1[i,k] mat2[k,j])
            multedMat

    let createMaped2Matr func matr1 matr2 =
        let rmatr1 = toArray matr1
        let rmatr2 = toArray matr2
        let resMatr = Array.map2 func rmatr1 rmatr2
        resMatr

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

    let testMap matr size func =
        let heigth = Array2D.length1 matr
        let wide = Array2D.length2 matr
        let expmatr = Array2D.map func matr
        let qmatr, qsize = createQtree matr         
        let mapedMatr = map func qmatr
        let actmatr = createFinalMatrix mapedMatr qsize heigth wide
        expmatr, actmatr

    let testMap2 matr1 matr2 size func =
        let expmatr = createMaped2Matr func matr1 matr2
        let qmatr1, qsize1 = createQtree matr1
        let qmatr2, qsize2 = createQtree matr2
        let actqmatr = map2 func qmatr1 qmatr2
        let actmatr2d = createFinalMatrix actqmatr qsize1 size size
        let actmatr = toArray actmatr2d
        let fequal = Array.forall2 (=) actmatr expmatr
        fequal

    let testMult matr1 matr2 size mulFunc addFunc =
        let expmatr= multMatr mulFunc addFunc matr1 matr2
        let qmatr1, qsize1 = createQtree matr1
        let qmatr2, qsize2 = createQtree matr2
        let actqmatr = multiplyMatrix mulFunc addFunc qmatr1 qsize1 qmatr2 qsize2
        let actmatr = createFinalMatrix actqmatr qsize1 size size
        expmatr, actmatr

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
        member _.intTest (size: uint) = 
            let matr = Gen.sample (int size) 1 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {-100000..100000}))
            let expmatr, actmatr = PropertyQtreeTest.testMap matr.[0] (int size) ((+) 1) 
            Assert.Equal(expmatr, actmatr)

        [<Property>]
        member _.charTest (size: uint) = 
            let matr = Gen.sample (int size) 1 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {' '..'~'}))
            let expmatr, actmatr = PropertyQtreeTest.testMap matr.[0] (int size) ((+) '1')
            Assert.Equal(expmatr, actmatr)

        [<Property>]
        member _.floatTest (size: uint) =
            let matr = Gen.sample (int size) 1 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {-infinityf..infinityf}))
            let expmatr, actmatr = PropertyQtreeTest.testMap matr.[0] (int size) ((+) 1f) 
            Assert.Equal(expmatr, actmatr)

    type map2Test()=

        [<Property>]    
        member _.intTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {-100000..100000}))
            let fequal = PropertyQtreeTest.testMap2 matr.[0] matr.[1] (int (int size))(+)
            Assert.True(fequal)

        [<Property>]
        member _.charTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {' '..'~'}))
            let fequal = PropertyQtreeTest.testMap2 matr.[0] matr.[1] (int (int size))(+)
            Assert.True(fequal)

        [<Property>]    
        member _.floatTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements{-infinityf..infinityf}))
            let fequal = PropertyQtreeTest.testMap2 matr.[0] matr.[1] (int (int size))(+)
            Assert.True(fequal)

    type multTest() =

        [<Property>]    
        member _.intTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {-100000..100000}))
            let expmatr, actmatr = PropertyQtreeTest.testMult matr.[0] matr.[1] (int (int size)) ( * ) (+)
            Assert.Equal(expmatr, actmatr)
        
        [<Property>]    
        member _.charTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {' ' .. '~'}))
            let expmatr, actmatr = PropertyQtreeTest.testMult matr.[0] matr.[1] (int (int size)) (fun x y -> char (int x * int y)) (+)
            Assert.Equal(expmatr, actmatr)
        [<Property>]    
        member _.floatTest (size: uint) =
            let matr = Gen.sample (int size) 2 (PropertyQtreeTest.matrixGenerator (int size) (int size) (Gen.elements {-infinityf .. infinityf}))
            let expmatr, actmatr = PropertyQtreeTest.testMult matr.[0] matr.[1] (int (int size)) ( * ) (+)
            Assert.Equal(expmatr, actmatr)