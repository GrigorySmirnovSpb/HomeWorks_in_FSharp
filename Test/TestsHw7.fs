namespace tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open QuattroMatrix
open QuattroMatrix.Matrix

module PropertyTest = 

    let toArray (mat: 'T array2d) : 'T array =
        let height = Array2D.length1 mat
        let width = Array2D.length2 mat
        Array.init (height * width) (fun i ->
            let row = i / width
            let col = i % width
            mat.[row, col]
        )

    let multMatr mat1 mat2 = 
        if Array2D.length1 mat1 <> Array2D.length2 mat2
        then failwith "Matrices have different sizes"
        else 
            let multedMat = Array2D.zeroCreate (Array2D.length1 mat1) (Array2D.length2 mat2)
            for i in 0 .. Array2D.length1 mat1 - 1 do
                for j in 0 .. Array2D.length2 mat1 - 1 do
                    for k in 0 .. Array2D.length2 mat2 - 1 do
                    multedMat[i,j] <- multedMat[i,j] + mat1[i,k] * mat2[k,j]
            multedMat

    let areAlmostEqual (a: float32) (b: float32) (epsilon: float32) =
        if Single.IsFinite(a) && Single.IsFinite(b) then
            abs (a - b) < epsilon
        elif Single.IsNaN(a) && Single.IsNaN(b) || Single.IsInfinity(a) && Single.IsInfinity(b) || 
            Single.IsNegativeInfinity(a) && Single.IsNegativeInfinity(b) || Single.IsPositiveInfinity(a) && Single.IsPositiveInfinity(b)
        then
            true
        else
            false

    let matrixGenerator (rows: int) (cols: int) (elementGen: Gen<'T>) : Gen<'T[,]> =
        let genMatrix =
            gen {
                let! elements = Gen.array2DOfDim rows cols elementGen
                return elements
            }
        genMatrix

   [<Properties(MaxTest = 100)>]
    type idTests() =

        [<Property>]
        member _.intTest (matr: int array2d) =
            let qudMatr = createSquareMatrix matr
            let qMatr = createQuadMatrix qudMatr 0 0 (Array2D.length1 qudMatr)
            let mapedMatr = map id qMatr
            Assert.Equal(qMatr, mapedMatr)

        [<Property>]
        member _.floatTest (matr: float array2d) =
            let qudMatr = createSquareMatrix matr
            let qMatr = createQuadMatrix qudMatr 0 0 (Array2D.length1 qudMatr)
            let mapedMatr = map id qMatr
            Assert.Equal(qMatr, mapedMatr)
        
        [<Property>]
        member _.charTest (matr: char array2d) = 
            let qudMatr = createSquareMatrix matr
            let qMatr = createQuadMatrix qudMatr 0 0 (Array2D.length1 qudMatr)
            let mapedMatr = map id qMatr
            Assert.Equal(qMatr, mapedMatr)
    
    type mapTest() =

        [<Property>]
        member _.intTest (matr: int array2d) = 
            let heigth = Array2D.length1 matr
            let wide = Array2D.length2 matr
            let expmatr = Array2D.map ((+) 1) matr
            let sqmatr = createSquareMatrix matr
            let qmatr = createQuadMatrix sqmatr 0 0 (Array2D.length1 sqmatr)
            let mapedMatr = map ((+) 1) qmatr
            let qmatr2d = toArray2d mapedMatr (Array2D.length1 sqmatr)
            let actmatr = Array2D.create heigth wide 0
            for i in 0 .. heigth - 1 do
                for j in 0 .. wide - 1 do
                    actmatr[i, j] <- qmatr2d[i, j]
            Assert.Equal(expmatr, actmatr)

        [<Property>]
        member _.charTest (matr: char array2d) = 
            let heigth = Array2D.length1 matr
            let wide = Array2D.length2 matr
            let expmatr = Array2D.map ((+) '1') matr
            let sqmatr = createSquareMatrix matr
            let qmatr = createQuadMatrix sqmatr 0 0 (Array2D.length1 sqmatr)
            let mapedMatr = map ((+) '1') qmatr
            let qmatr2d = toArray2d mapedMatr (Array2D.length1 sqmatr)
            let actmatr = Array2D.create heigth wide '1'
            for i in 0 .. heigth - 1 do
                for j in 0 .. wide - 1 do
                    actmatr[i, j] <- qmatr2d[i, j]
            Assert.Equal(expmatr, actmatr)

    type map2Test()=

        [<Property>]    
        member _.intTest (size: uint) =
            let matr1 = Gen.sample 1 (PropertyTest.matrixGenerator (int size) (int size) (Gen.choose (-100000, 100000)))
            let matr2 = Gen.sample 1 (PropertyTest.matrixGenerator (int size) (int size) (Gen.choose (-100000, 100000)))
            let matr11d = PropertyTest.toArray matr1.[0]
            let matr21d = PropertyTest.toArray matr2.[0]
            let expmatr = Array.map2 (fun x y -> x + y) matr11d matr21d    
            let sqmatr1 = createSquareMatrix matr1.[0]
            let qmatr1 = createQuadMatrix sqmatr1 0 0 (Array2D.length1 sqmatr1)
            let sqmatr2 = createSquareMatrix matr2.[0]
            let qmatr2 = createQuadMatrix sqmatr2 0 0 (Array2D.length1 sqmatr2)
            let actqmatr = map2 (+) qmatr1 qmatr2
            let qmatr2d = toArray2d actqmatr  (Array2D.length1 sqmatr1)
            let actmatr2d = Array2D.create (int size) (int size) 0
            for i in 0 .. (int size) - 1 do
                for j in 0 .. (int size) - 1 do
                    actmatr2d[i, j] <- qmatr2d[i, j]
            let actmatr = PropertyTest.toArray actmatr2d
            let fequal = Array.forall2 (=) actmatr expmatr
            Assert.True(fequal)

        [<Property>]
        member _.charTest (size: uint) =
            let matr1 = Gen.sample 1 (PropertyTest.matrixGenerator (int size) (int size) (Gen.elements {' ' .. '~'}))
            let matr2 = Gen.sample 1 (PropertyTest.matrixGenerator (int size) (int size) (Gen.elements {' ' .. '~'}))
            let matr11d = PropertyTest.toArray matr1.[0]
            let matr21d = PropertyTest.toArray matr2.[0]
            let expmatr = Array.map2 (fun x y -> x + y) matr11d matr21d    
            let sqmatr1 = createSquareMatrix matr1.[0]
            let qmatr1 = createQuadMatrix sqmatr1 0 0 (Array2D.length1 sqmatr1)
            let sqmatr2 = createSquareMatrix matr2.[0]
            let qmatr2 = createQuadMatrix sqmatr2 0 0 (Array2D.length1 sqmatr2)
            let actqmatr = map2 (+) qmatr1 qmatr2
            let qmatr2d = toArray2d actqmatr (Array2D.length1 sqmatr1)
            let actmatr2d = Array2D.create (int size) (int size) ' '
            for i in 0 .. (int size) - 1 do
                for j in 0 .. (int size) - 1 do
                    actmatr2d[i, j] <- qmatr2d[i, j]
            let actmatr = PropertyTest.toArray actmatr2d
            let fequal = Array.forall2 (=) actmatr expmatr
            Assert.True(fequal)
    type multTest() =

        [<Property>]    
        member _.intTest (size: uint) =
            let matr1 = Gen.sample 1 (PropertyTest.matrixGenerator (int size) (int size) (Gen.choose (-100000, 100000)))
            let matr2 = Gen.sample 1 (PropertyTest.matrixGenerator (int size) (int size) (Gen.choose (-100000, 100000)))
            let expmatr= PropertyTest.multMatr matr1.[0] matr2.[0]
            let sqmatr1 = createSquareMatrix matr1.[0]
            let qmatr1 = createQuadMatrix sqmatr1 0 0 (Array2D.length1 sqmatr1)
            let sqmatr2 = createSquareMatrix matr2.[0]
            let qmatr2 = createQuadMatrix sqmatr2 0 0 (Array2D.length1 sqmatr2)
            let actqmatr = multiplyMatrix qmatr1 (Array2D.length1 sqmatr1) qmatr2 (Array2D.length1 sqmatr2)
            let qmatr2d = toArray2d actqmatr  (Array2D.length1 sqmatr1)
            let actmatr = Array2D.create (int size) (int size) 0
            for i in 0 .. (int size) - 1 do
                for j in 0 .. (int size) - 1 do
                    actmatr[i, j] <- qmatr2d[i, j]
            Assert.Equal(expmatr, actmatr)
