namespace Benchmarks

open System
open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Columns
open Perfolizer.Horology

open LibSorting.Sorts
open ListLib.MyList

type ArrayBenchmark() =

    static member ArrayLengths = [|10000..10000..100000|]
    member this.Random = Random()

    [<ParamsSource("ArrayLengths")>]
    member val ArrayLength = 0 with get, set

    member val ArrayToSort = [|0.0|] with get, set

    [<IterationSetup>]
    member this.GetArrayToSort () =
        this.ArrayToSort <- Array.init this.ArrayLength (fun _ -> this.Random.NextDouble())

    [<Benchmark(Baseline = true)>]
    member this.MergeBenchArray () = MergeSort this.ArrayToSort compare

    [<Benchmark>]
    member this.QuickBenchArray () = QuickSort this.ArrayToSort compare

    [<Benchmark>]
    member this.BubbleBenchArray () = Bubblesort this.ArrayToSort compare

    [<Benchmark>]
    member this.SysBenchArray () = Array.sort this.ArrayToSort

type ListBenchmark() =

    static member ListLengths = [|10000..10000..100000|]
    member this.Random = Random()

    [<ParamsSource("ListLengths")>]
    member val ListLength = 0 with get, set
    member val ListToSort = [0.0] with get, set

    [<IterationSetup>]
    member this.GetListToSort () = 
        this.ListToSort <- List.init this.ListLength (fun _ -> this.Random.NextDouble()) 

    [<Benchmark(Baseline = true)>]
    member this.MergeBenchList () = mergeSort (fromList this.ListToSort) compare

    [<Benchmark>]
    member this.QuickBenchList () = quickSort (fromList this.ListToSort) compare

    [<Benchmark>]
    member this.BubbleBenchList () = bubbleSort (fromList this.ListToSort) compare

    [<Benchmark>]
    member this.SysBenchList () = List.sort this.ListToSort

module Main =
    [<EntryPoint>]
    let main argv =

        let config = ManualConfig
                        .Create(DefaultConfig.Instance)
                        .WithSummaryStyle(SummaryStyle.Default.WithTimeUnit(TimeUnit.Millisecond))
                        .WithOptions(ConfigOptions.DisableOptimizationsValidator)
                        .AddColumn(StatisticColumn.Median)
        let benchmarks =
            BenchmarkSwitcher [| typeof<ArrayBenchmark>; typeof<ListBenchmark> |]

        benchmarks.Run argv |> ignore
        0
