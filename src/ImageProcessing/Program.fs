open ImageProcessing
open Argu

let pathToExamples = "/home/gregory/demo_2024/FsharpProj/src/ImageProcessing/pics/"
let inputFolder = System.IO.Path.Combine(pathToExamples, "input")

// Определяем перечисление для фильтров
type Filter =
    | SmallSharpness = 1
    | Edges = 2
    | Beautiful = 3
    | GaussianBlur = 4
    | Sobel = 5
    | BigSharpness = 6
    | Id = 7
    | Black = 8

type CmdArgs =
    | [<GatherUnrecognized>] Input_File of string
    | [<GatherAllSources>] Out_File of string
    | [<Last>] Filter of Filter

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
    
    if results.Contains Input_File
        && results.Contains Out_File
        && results.Contains Filter
    then
        let inFile = results.GetResult Input_File
        let outFile = results.GetResult Out_File
        let filter = results.GetResult Filter

        if not (System.IO.File.Exists(inFile)) then
            failwith "Error: Input file does not exist."

        let inImage = loadAs2DArray inFile

        let resultImage =
            match filter with
            | Filter.SmallSharpness -> applyFilter smallsharpnessKernel inImage
            | Filter.Edges -> applyFilter edgesKernel inImage
            | Filter.Beautiful -> applyFilter beautifulKernel inImage
            | Filter.GaussianBlur -> applyFilter gaussianBlurKernel inImage
            | Filter.Sobel -> applyFilter sobelKernel inImage
            | Filter.BigSharpness -> applyFilter bigsharpnessKernel inImage
            | Filter.Id -> applyFilter idKernel inImage
            | Filter.Black -> applyFilter blackKernel inImage

        save2DByteArrayAsImage resultImage outFile

    elif argv = [|"help"|] then 
        let usage = parser.PrintUsage()
        printfn "%s" usage
    else 
        printfn "Missing args\n Given: %A, but needed: --input-file <filename> --out-file <filename> --filter <number of filter>" argv

    0
