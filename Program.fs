open System
open Graf

let version = "0.1.0"

[<EntryPoint>]
let main argv =
    // parse CLI options
    let width = 80
    let height = 24

    // compute derived parameters
    let m = height - 2 - 2
    if m < 1 then failwithf $"{width}x{height} plot region is not tall enough"
    let n = width - (7 + 3 + 1)
    if n < 1 then failwithf $"{width}x{height} plot region is not wide enough"

    // allocate buffers
    let data = StatsQueue.make n
    let labels = Array.create m ""

    // define update loop
    let redraw data =
        // update stats
        let min = StatsQueue.min data
        let max = StatsQueue.max data
        let avg = StatsQueue.avg data
        let std = StatsQueue.std data
        let nans = StatsQueue.dropped data

        // prepare Y axis labels
        let yaxis i = Math.lerp (0.0, float m - 1.0) (min, max) (float i)
        for i = 0 to labels.Length - 1 do
            labels[i] <- sprintf "% 10.3g " (yaxis i)

        // plot and redraw the graph
        let p = plot (m, n) (StatsQueue.toSeq data)
        do
            Console.Clear()
            printfn $"    github.com/baioc/graf v{version}\n"
            Console.WriteLine(Plot.toString p labels)
            printfn "\n    avg=%.2g std=%.2g nans=%d" avg std nans

    // drive update loop
    let mutable input = Console.In.ReadLine()
    while not (isNull input) do
        let y = match Double.TryParse(input) with true, x -> x | _ -> nan
        do StatsQueue.enqueue data y
        if StatsQueue.length data > 0 then redraw data
        input <- Console.In.ReadLine()

    // returns the number of input lines which failed to parse as a data point
    StatsQueue.dropped data
