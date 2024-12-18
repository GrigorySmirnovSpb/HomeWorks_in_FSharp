namespace tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open ImageProcessing
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

module PropertyTests =

    let RandomPics =
        let rand = new Random()
        let height = rand.Next(1, 500)
        let width = rand.Next(1, 500)

        Array2D.init height width (fun j i ->

            { r = byte (rand.Next(0, 256))
              g = byte (rand.Next(0, 256))
              b = byte (rand.Next(0, 256)) })
    (*for i in 0 .. height - 1 do
            for j in 0 .. width - 1 do
                let pixR = byte (rand.Next(0, 256))
                let pixG = byte (rand.Next(0, 256))
                let pixB = byte (rand.Next(0, 256))*)


    [<Properties(MaxTest = 100)>]

    type idTests() =
        [<Property>]
        member _.idTest() = //Данный тест показывает что примение фильтра не меняет размер исходного изображения (в текущем примере матрицы, котрая играет роль изображения), также можно сказать, что и следующие тесты обладают таким свойством
            let img = RandomPics
            Assert.Equal(img, applyFilter idKernel img)

    type filterTests() =

        let prodMatZn (filter: float32[][]) x y matr =
            let height = Array.length filter
            let width = Array.length filter.[0]
            let delta = height / 2
            let mutable sumR = 0.0f
            let mutable sumG = 0.0f
            let mutable sumB = 0.0f

            for i in 0 .. height - 1 do
                for j in 0 .. width - 1 do
                    let value = filter.[i].[j]
                    let ix = x - delta + i
                    let jy = y - delta + j

                    if ix >= 0 && jy >= 0 && ix < (Array2D.length1 matr) && jy < (Array2D.length2 matr) then
                        sumR <- sumR + (float32 matr[ix, jy].r * value)
                        sumG <- sumG + (float32 matr[ix, jy].g * value)
                        sumB <- sumB + (float32 matr[ix, jy].b * value)
                    else
                        sumR <- sumR + (float32 matr[x, y].r * value)
                        sumG <- sumG + (float32 matr[x, y].g * value)
                        sumB <- sumB + (float32 matr[x, y].b * value)

            { r = byte (min 255 (max 0 (int sumR)))
              g = byte (min 255 (max 0 (int sumG)))
              b = byte (min 255 (max 0 (int sumB))) }

        [<Property>]
        member _.EdgefilterTest() =
            let matr = RandomPics
            let expmatr = Array2D.mapi (fun x y _ -> prodMatZn edgesKernel x y matr) matr
            let actmatr = applyFilter edgesKernel matr
            Assert.Equal(actmatr, expmatr)

        [<Property>]
        member _.GaussfilterTest() =
            let matr = RandomPics
            let expmatr = Array2D.mapi (fun x y _ -> prodMatZn gaussianBlurKernel x y matr) matr
            let actmatr = applyFilter gaussianBlurKernel matr
            Assert.Equal(actmatr, expmatr)

        [<Property>]
        member _.BeautyfilterTest() =
            let matr = RandomPics
            let expmatr = Array2D.mapi (fun x y _ -> prodMatZn beautifulKernel x y matr) matr
            let actmatr = applyFilter beautifulKernel matr
            Assert.Equal(actmatr, expmatr)

        [<Property>]
        member _.SmallSharpfilterTest() =
            let matr = RandomPics

            let expmatr =
                Array2D.mapi (fun x y _ -> prodMatZn smallsharpnessKernel x y matr) matr

            let actmatr = applyFilter smallsharpnessKernel matr
            Assert.Equal(actmatr, expmatr)

        [<Property>]
        member _.BigSharpfilterTest() =
            let matr = RandomPics
            let expmatr = Array2D.mapi (fun x y _ -> prodMatZn bigsharpnessKernel x y matr) matr
            let actmatr = applyFilter bigsharpnessKernel matr
            Assert.Equal(actmatr, expmatr)

