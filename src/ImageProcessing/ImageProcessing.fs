module ImageProcessing

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

type Rgb = { r: byte; g: byte; b: byte }

[<Struct>]
type Image =
    val Data: array<Rgb>
    val Width: int
    val Height: int
    val Name: string

    new(data, width, height, name) =
        { Data = data
          Width = width
          Height = height
          Name = name }

let loadAs2DArray (file: string) =
    let img = Image.Load<Rgb24> file

    let res =
        Array2D.init img.Height img.Width (fun j i ->
            let pixel = img.[i, j]

            { r = pixel.R
              g = pixel.G
              b = pixel.B })

    printfn $"H=%A{img.Height} W=%A{img.Width}"
    printfn $"%A{img.Metadata}"
    res

let save2DByteArrayAsImage (imageData: Rgb[,]) file =
    let h = imageData.GetLength 0
    let w = imageData.GetLength 1
    printfn $"H=%A{h} W=%A{w}"

    let flat2Darray array2D =
        [| for x in [ 0 .. (Array2D.length1 array2D) - 1 ] do
               for y in [ 0 .. (Array2D.length2 array2D) - 1 ] do
                   yield array2D.[x, y] |]

    // Преобразование Rgb в Rgb24
    let flatRgb24Array =
        flat2Darray imageData |> Array.map (fun rgb -> Rgb24(rgb.r, rgb.g, rgb.b))

    // Создание изображения из одномерного массива
    let img = Image.LoadPixelData<Rgb24>(flatRgb24Array, w, h)
    img.Save(file)

let gaussianBlurKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 4; 16; 24; 16; 4 |]
       [| 6; 24; 36; 24; 6 |]
       [| 4; 16; 24; 16; 4 |]
       [| 1; 4; 6; 4; 1 |] |]
    |> Array.map (Array.map (fun x -> (float32 x) / 256.0f))

let edgesKernel =
    [| [| 0; 0; -1; 0; 0 |]
       [| 0; 0; -1; 0; 0 |]
       [| 0; 0; 2; 0; 0 |]
       [| 0; 0; 0; 0; 0 |]
       [| 0; 0; 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let sobelKernel =
    [| [| 1; 0; -1 |];
       [| 2; 0; -2 |];
       [| 1; 0; -1 |] |]
    |> Array.map (Array.map float32)

let smallsharpnessKernel =
    [| [| 0; -1; 0 |];
       [| -1; 5; -1 |];
       [| 0; -1; 0 |] |]
    |> Array.map (Array.map float32)

let bigsharpnessKernel =
    [| [| -1; -1; -1 |];
       [| -1; 9; -1 |];
       [| -1; -1; -1 |] |]
    |> Array.map (Array.map float32)

let extbigsharpnessKernel =
    [| [| 0;  0;  0;  0; 0 |];
       [| 0; -1; -1; -1; 0 |];
       [| 0; -1;  9; -1; 0 |];
       [| 0; -1; -1; -1; 0 |];
       [| 0;  0;  0;  0; 0 |]  |]
    |> Array.map (Array.map float32)

let beautifulKernel =
    [| [| -1; -1; -1 |];
       [| -1; 8; -1 |];
       [| -1; -1; -1 |] |]
    |> Array.map (Array.map float32)

let idKernel =
    [| [| 0; 0; 0 |];
       [| 0; 1; 0 |];
       [| 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let blackKernel =
    [| [| 0; 0; 0 |];
       [| 0; 0; 0 |];
       [| 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let shiftRight =
    [| [| 0; 0; 0 |];
       [| 0; 0; 1 |];
       [| 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let shiftDown =
    [| [| 0; 0; 0 |];
       [| 0; 0; 0 |];
       [| 0; 1; 0 |] |]
    |> Array.map (Array.map float32)

let shiftDiagonal =
    [| [| 0; 0; 0 |];
       [| 0; 0; 0 |];
       [| 0; 0; 1 |] |]
    |> Array.map (Array.map float32)

let applyFilter (filter: float32[][]) (img: Rgb[,]) =
    let imgH = img.GetLength 0
    let imgW = img.GetLength 1

    let filterD = (Array.length filter) / 2

    let processPixel px py =
        let mutable sumR = 0.0f
        let mutable sumG = 0.0f
        let mutable sumB = 0.0f
        let mutable x = 0
        let mutable y = 0

        for i in px - filterD .. px + filterD do
            for j in py - filterD .. py + filterD do
                if i >= 0 && i < imgH && j >= 0 && j < imgW then
                    let pixel = img.[i, j]
                    let value = filter.[x].[y]
                    sumR <- sumR + (float32 pixel.r * value)
                    sumG <- sumG + (float32 pixel.g * value)
                    sumB <- sumB + (float32 pixel.b * value)
                y <- y + 1

            x <- x + 1
            y <- 0

        // Ограничиваем значения RGB в диапазоне [0, 255]
        { r = byte (min 255 (max 0 (int sumR)))
          g = byte (min 255 (max 0 (int sumG)))
          b = byte (min 255 (max 0 (int sumB))) }

    Array2D.mapi (fun x y _ -> (processPixel x y)) img
