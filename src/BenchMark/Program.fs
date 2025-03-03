namespace Benchmarks

open System
open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Columns
open Perfolizer.Horology

open LibSorting.Sorts

type SortsBenchmark() =
    
    static member ArrayLengths = [|10000..10000..100000|]
    member this.Random = Random()
    
    [<ParamsSource("ArrayLengths")>]
    member val ArrayLength = 0 with get, set

    member val ArrayToSort = [|0.0|] with get, set

    [<IterationSetup>]
    member this.GetArrayToSort () = 
        this.ArrayToSort <- Array.init this.ArrayLength (fun _ -> this.Random.NextDouble()) 

    [<Benchmark(Baseline = true)>]
    member this.MergeBench () = MergeSort this.ArrayToSort compare

    [<Benchmark>]
    member this.QuickBench () = QuickSort this.ArrayToSort compare

    [<Benchmark>]
    member this.BubbleBench () = Bubblesort this.ArrayToSort compare

    [<Benchmark>]
    member this.SysBench () = Array.sort this.ArrayToSort

module Main =
    [<EntryPoint>]
    let main argv =
        
        let config = ManualConfig
                        .Create(DefaultConfig.Instance)
                        .WithSummaryStyle(SummaryStyle.Default.WithTimeUnit(TimeUnit.Millisecond)) 
                        .WithOptions(ConfigOptions.DisableOptimizationsValidator)
                        .AddColumn(StatisticColumn.Median)
        let benchmarks =
            BenchmarkSwitcher [| typeof<SortsBenchmark> |]

        benchmarks.Run argv |> ignore
        0