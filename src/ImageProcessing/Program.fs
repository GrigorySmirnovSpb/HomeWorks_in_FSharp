open System
open System.Drawing
open Microsoft.FSharp.Collections
open Argu

type CliArguments =
    | Working_Directory of path: string
    | Listener of host: string * port: int
    | Data of base64: byte[]
    | Port of tcp_port: int
    | Log_Level of level: int
    | Detach

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "specify a working directory."
            | Listener _ -> "specify a listener (hostname : port)."
            | Data _ -> "binary data in base64 encoding."
            | Port _ -> "specify a primary port."
            | Log_Level _ -> "set the log level."
            | Detach -> "detach daemon from console."

module ImageProcessing =

    let filter1 = array2D [ [ 0.0; 0.0; 0.0 ]; [ 0.0; 1.0; 0.0 ]; [ 0.0; 0.0; 0.0 ] ]

    let coef = 1.0
    let bias = 0.0
    type ColorRGB = { R: byte; G: byte; B: byte }

    let LoadImage(* (filename: string)*) : ColorRGB array2d =

        use bitmap = new Bitmap("src/ImageProcessing/pics/flower.png", true)
        let width = bitmap.Width
        let height = bitmap.Height // Исправлено на Height
        let image = Array2D.init height width (fun i j -> { R = 0uy; G = 0uy; B = 0uy }) // Исправлено на Color.FromArgb

        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let color = bitmap.GetPixel(x, y)

                image[y, x] <-
                    { R = color.R
                      G = color.G
                      B = color.B }

        image // Исправлено на height

    let ApplieFilter (filter: float array2d) (image: ColorRGB array2d) =

        let width = Array2D.length2 image
        let height = Array2D.length1 image

        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let mutable red, green, blue = 0.0, 0.0, 0.0

                for fy in 0..2 do
                    for fx in 0..2 do
                        let imageX = (x - 1 + fx + width) % width
                        let imageY = (y - 1 + fy + height) % height
                        red <- red + float image[imageY, imageX].R * filter[fy, fx]
                        green <- green + float image[imageY, imageX].G * filter[fy, fx]
                        blue <- blue + float image[imageY, imageX].B * filter[fy, fx]

                image[y, x] <-
                    { R = byte (Math.Min(Math.Max(int (coef * red + bias), 0), 255))
                      G = byte (Math.Min(Math.Max(int (coef * green + bias), 0), 255))
                      B = byte (Math.Min(Math.Max(int (coef * blue + bias), 0), 255)) }

        image

    let CreateImage (image: ColorRGB array2d) (filename: string) =

        let width = Array2D.length2 image
        let height = Array2D.length1 image
        use bitmap = new Bitmap(width, height)

        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let color = Color.FromArgb(int image[y, x].R, int image[y, x].G, int image[y, x].B)
                bitmap.SetPixel(x, y, color)

        bitmap.Save(filename)


    [<EntryPoint>]
    let main args =
        // Здесь можно вызвать LoadImage или выполнить другие действия
        let mutable image = LoadImage
        image <- ApplieFilter filter1 image
        CreateImage image args.[1]
        0
