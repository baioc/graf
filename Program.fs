open System
open Graf

let version = "0.1.b"

[<EntryPoint>]
let main argv =
    // parse CLI options
    let title = ""
    let floatDigits = 3
    let width = Console.BufferWidth
    let height = Console.BufferHeight
    let clearWidth = String.replicate width " "

    // compute derived parameters
    let wastedLines = 0 + 2 + 1 // title + stats + newline
    let m = height - wastedLines
    if m < 2 then do
        Console.Error.WriteLine $"{width} x {height} plot region is not tall enough"
        exit -1
    let maxFloatCrap = 7 // sign + dot + e + eSign + 3 exponent digits
    let space = 1
    let n = width - (maxFloatCrap + floatDigits) - space
    if n < 2 then do
        Console.Error.WriteLine $"{width} x {height} plot region is not wide enough"
        exit -1
    let labelFormat = $"{{0,{maxFloatCrap + floatDigits}:g{floatDigits}}} "
    let statsFormat = clearWidth + $"\r    avg={{0:g{floatDigits}}} std={{1:g{floatDigits}}} nans={{2:d}}"
    let title = clearWidth + $"\r    " + title

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
            labels[i] <- String.Format(labelFormat, yaxis i)

        // plot and redraw the chart
        let plot = Graf.plot (m, n) AnsiColor.MAGENTA (StatsQueue.toSeq data)
        do
            Console.SetCursorPosition(0, 0)
            Console.Out.WriteLine (Plot.toString labels plot)
            Console.Out.WriteLine clearWidth
            Console.Out.WriteLine(statsFormat, avg, std, nans)

    // drive update loop
    let mutable input = Console.In.ReadLine()
    do // with hidden cursor, but ensuring it gets re-enabled on exit
        Console.Clear()
        Console.CancelKeyPress.Add (fun _ -> Console.CursorVisible <- true)
        Console.CursorVisible <- false
    while not (isNull input) do
        let y = match Double.TryParse(input) with true, x -> x | _ -> nan
        do StatsQueue.enqueue data y
        if StatsQueue.length data > 0 then redraw data
        input <- Console.In.ReadLine()
    do Console.CursorVisible <- true

    // returns the number of input lines which failed to parse as a data point
    StatsQueue.dropped data
