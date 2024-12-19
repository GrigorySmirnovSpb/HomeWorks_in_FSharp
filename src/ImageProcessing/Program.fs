open ImageProcessing
open Argu

let pathToExamples = "/home/gregory/demo_2024/FsharpProj/src/ImageProcessing/pics/"
let inputFolder = System.IO.Path.Combine(pathToExamples, "input")

type CmdArgs =
    | [<Mandatory>] Input_File of string
    | [<Mandatory>] Out_File of string
    | [<Mandatory>] Filter of string
    | [<AltCommandLine("-help")>] Inf

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Input_File _ -> "File to process."
            | Out_File _ -> "Where to save result."
            | Inf -> "Display help information."
            | Filter _ ->
                "Filter which you want to use:\n 1: smallsharpness\n 2: edges\n 3: beautiful (black with sharp)\n 4: gaussianblur\n 5: sobel\n 6: bigsharpness\n 7: id"

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    //printfn "%s" "Run program with arg [--inf] of [-help], if you want check help information"
    let parser = ArgumentParser.Create<CmdArgs>(programName = "ImageProcessing")

    let results = parser.Parse argv

    if results.Contains Inf then
        printfn "%s" (parser.PrintUsage())
    elif
        results.Contains Input_File
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
            | "1" -> applyFilter smallsharpnessKernel inImage
            | "2" -> applyFilter edgesKernel inImage
            | "3" -> applyFilter beautifulKernel inImage
            | "4" -> applyFilter gaussianBlurKernel inImage
            | "5" -> applyFilter sobelKernel inImage
            | "6" -> applyFilter bigsharpnessKernel inImage
            | "7" -> applyFilter idKernel inImage
            | _ -> failwith "No such filter"

        save2DByteArrayAsImage resultImage outFile

    0
