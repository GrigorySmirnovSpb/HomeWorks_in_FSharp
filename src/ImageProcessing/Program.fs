open ImageProcessing
open Argu

let pathToExamples = "/home/gregory/demo_2024/FsharpProj/src/ImageProcessing/pics/"
let inputFolder = System.IO.Path.Combine(pathToExamples, "input")

let demoFile =
    System.IO.Path.Combine(inputFolder, "armin-djuhic-ohc29QXbS-s-unsplash.jpg")

type CmdArgs =
    | [<Mandatory>] Input_File of string
    | [<Mandatory>] Out_File of string
    | [<Mandatory>] Filter of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with 
            | Input_File _ -> "File to process."
            | Out_File _ -> "Where to save result."
            | Filter _ -> 
            "Filter witch you want use\n 1: smallsharness\n 2: edges\n 3: beautiful (black with sharp)\n 4: gaussianblur\n 5: sobel\n 6: bigsharpness\n blackwhite"

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let parser = ArgumentParser.Create<CmdArgs>(programName = "ImageProcessing")
    let usage = parser.PrintUsage()
    let results = parser.Parse argv
    let inFile = results.GetResult Input_File
    let outFile = results.GetResult Out_File
    let nameFilter = results.GetResult Filter
    let inImage = loadAs2DArray inFile
    let resultImage = 
        match nameFilter with
        | "1" -> applyFilter smallsharpnessKernel inImage
        | "2" -> applyFilter edgesKernel inImage
        | "3" -> applyFilter beautifulKernel inImage
        | "4" -> applyFilter gaussianBlurKernel inImage
        | "5" -> applyFilter sobelKernel inImage
        | "6" -> applyFilter bigsharpnessKernel inImage
        | "7" -> applyFilter bwKernel inImage
        | _ -> failwith "No this filter"
        //applyFilter sharpnessKernel inImage
        //applyFilter edgesKernel inImage
        //applyFilter gaussianBlurKernel inImage
        //applyFilter sobelKernel inImage
        //applyFilter somefilter inImage
        //applyFilter gaussianBlur2 inImage
    save2DByteArrayAsImage resultImage outFile
    0