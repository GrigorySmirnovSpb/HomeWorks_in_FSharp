open ImageProcessing
open Argu

let pathToExamples = "/home/gregory/demo_2024/FsharpProj/src/ImageProcessing/pics/"
let inputFolder = System.IO.Path.Combine(pathToExamples, "input")

type CmdArgs =
    | [<GatherUnrecognized>] Input_File of string
    | [<GatherAllSources>] Out_File of string
    | [<Last>] Filter of int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Input_File _ -> "File to process."
            | Out_File _ -> "Where to save result."
            | Filter _ ->
                "Filter which you want to use:\n 1: smallsharpness\n 2: edges\n 3: beautiful (black with sharp)\n 4: gaussianblur\n 5: sobel\n 6: bigsharpness\n 7: id\n 8: black"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CmdArgs>(programName = "ImageProcessing")

    let results = parser.Parse argv
    
    if  results.Contains Input_File
        && results.Contains Out_File
        && results.Contains Filter
    then
        let inFile = results.GetResult Input_File
        let outFile = results.GetResult Out_File
        let nameFilter = results.GetResult Filter

        if not (System.IO.File.Exists(inFile)) then
            failwith "Error: Input file does not exist."

        let inImage = loadAs2DArray inFile

        let resultImage =
            match nameFilter with
            | 1 -> applyFilter smallsharpnessKernel inImage
            | 2 -> applyFilter edgesKernel inImage
            | 3 -> applyFilter beautifulKernel inImage
            | 4 -> applyFilter gaussianBlurKernel inImage
            | 5 -> applyFilter sobelKernel inImage
            | 6 -> applyFilter bigsharpnessKernel inImage
            | 7 -> applyFilter idKernel inImage
            | 8 -> applyFilter blackKernel inImage
            | _ -> failwith "No such filter\n"

        save2DByteArrayAsImage resultImage outFile

    elif argv = [|"help"|]
    then 
        let usage = parser.PrintUsage()
        printfn "%s" usage
    elif results.Contains Input_File
        || results.Contains Out_File
        || results.Contains Filter
    then printfn "Missing args\n Given: %A, but neded: --input-file <filename> --out-file <filename> --filter <number of filter>" argv
    else printfn "Usage: 'dotnet run -- help' to see help information"

    0
