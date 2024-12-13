open ImageProcessing
open Argu

let pathToExamples = "/home/gregory/demo_2024/FsharpProj/src/ImageProcessing/pics/"
let inputFolder = System.IO.Path.Combine(pathToExamples, "input")

let demoFile =
    System.IO.Path.Combine(inputFolder, "armin-djuhic-ohc29QXbS-s-unsplash.jpg")

type CmdArgs =
    | [<Mandatory>] Input_File of string
    | [<Mandatory>] Out_File of string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Input_File _ -> "File to process."
            | Out_File _ -> "Where to save result."

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    let parser = ArgumentParser.Create<CmdArgs>(programName = "ImageProcessing")
    let usage = parser.PrintUsage()
    let results = parser.Parse argv
    let inFile = results.GetResult Input_File
    let outFile = results.GetResult Out_File
    let inImage = loadAs2DArray inFile

    let resultImage =
        //applyFilter sharpnessKernel inImage
        //applyFilter edgesKernel inImage
        applyFilter gaussianBlurKernel inImage
    //|> applyFilter gaussianBlurKernel
    //|> applyFilter gaussianBlurKernel
    //|> applyFilter gaussianBlurKernel
    //|> applyFilter edgesKernel
    save2DByteArrayAsImage resultImage outFile
    0
