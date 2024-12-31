namespace tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open ImageProcessing

module PropertyTests =

    [<Properties(MaxTest = 100)>]

    type idTests() =
        [<Property>]
        member _.idTest(img: Rgb array2d) = 
            Assert.Equal(img, applyFilter idKernel img)

    type filterTests() =

        [<Property>]
        member _.EdgefilterTest(matr: Rgb array2d) =
            let actmatr = applyFilter blackKernel matr
            let mutable sum = 0
            for i in 0 .. Array2D.length1 actmatr - 1 do
                for j in 0 .. Array2D.length2 actmatr - 1 do
                    sum <- sum + int actmatr[i, j].r + int actmatr[i, j].g + int actmatr[i, j].b      
            Assert.Equal(0, sum)
        
        [<Property>]
        member _.sizeTest(img: Rgb array2d) = 
            let actmatr = applyFilter beautifulKernel img
            Assert.Equal(Array2D.length1 img = Array2D.length1 actmatr, Array2D.length2 img = Array2D.length2 actmatr)
