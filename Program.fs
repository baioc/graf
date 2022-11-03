open System
open System.Globalization

open Handmade.Collections.Generic

open Graf


module Seq =
    /// Returns element count, min, max, average and (sample) variance of a given seq.
    let statistics seq =
        let init = (0, Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0.0)
        let acc (n, m, M, s, ss) x = (n + 1, min m x, max M x, s + x, ss + x*x)
        let len, min, max, sum, squaredSum = seq |> Seq.fold acc init
        let n = float len
        let avg = sum / n
        let var = (squaredSum - (sum * sum / n )) / (n - 1.0)
        len, min, max, avg, var


let version = "0.2.7"

let usage = $"\
Usage: graf [OPTION]...
    Plot discretized line charts in your terminal.

Options:
    -h, --help           Print this help message and exit the program.
    -f, --file FILE      If FILE is - or not specified, read from stdin.
    -t, --title TITLE    Display TITLE on top of the plotted chart.
    -c, --color COLOR    Color to plot the line in. See options below.
    -s, --stats          Show statistics, which are hidden by default.
    -r, --range MIN:MAX  Fix plot bounds instead of choosing them dynamically.
    -d, --digits DIGITS  Ensure at least DIGITS significant digits are printed.
    -W, --width WIDTH    Maximum TUI width. Defaults to terminal width.
    -H, --height HEIGHT  Maximum TUI height. Defaults to terminal height.

Colors:
    \x1b[30m k, black \x1b[0m \t \x1b[31m r, red     \x1b[0m \t \x1b[32m g, green \x1b[0m \t \x1b[33m y, yellow \x1b[0m
    \x1b[34m b, blue  \x1b[0m \t \x1b[35m m, magenta \x1b[0m \t \x1b[36m c, cyan  \x1b[0m \t \x1b[37m w, white  \x1b[0m

Links:
    Project page  https://github.com/baioc/graf
    Bug tracker   https://github.com/baioc/graf/issues

graf v{version}
Copyright (c) 2022 Gabriel B. Sant'Anna"

[<EntryPoint>]
let main argv =
    do CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture

    // parse command line
    let help = false
    let file = "-"
    let title = ""
    let color = "blue"
    let stats = false
    let range = ":"
    let digits = 1
    let width = Console.BufferWidth
    let height = Console.BufferHeight

    // apply options
    if help then
        do Console.Out.WriteLine(usage); exit 0
    if file <> "-" then
        try
            do Console.SetIn(new IO.StreamReader(file))
        with | _ ->
            do Console.Error.WriteLine($"Could not open file '{file}'"); exit -1
    let color =
        let maybeColor =
            Map.tryFind color (Map.ofSeq [
                ("", AnsiColor.DEFAULT)
                ("k", AnsiColor.BLACK); ("black", AnsiColor.BLACK)
                ("r", AnsiColor.RED); ("red", AnsiColor.RED)
                ("g", AnsiColor.GREEN); ("green", AnsiColor.GREEN)
                ("y", AnsiColor.YELLOW); ("yellow", AnsiColor.YELLOW)
                ("b", AnsiColor.BLUE); ("blue", AnsiColor.BLUE)
                ("m", AnsiColor.MAGENTA); ("magenta", AnsiColor.MAGENTA)
                ("c", AnsiColor.CYAN); ("cyan", AnsiColor.CYAN)
                ("w", AnsiColor.WHITE); ("white", AnsiColor.WHITE)
            ])
        if Option.isNone maybeColor then do Console.Error.WriteLine($"Unknown color {color}"); exit -1
        maybeColor.Value
    let lowerBound, upperBound =
        if range = "" then
            Double.NegativeInfinity, Double.PositiveInfinity
        else
            try
                let subs = range.Split(':')
                if subs.Length <> 2 then failwith range
                let lo, hi = subs[0], subs[1]
                Double.Parse(lo), Double.Parse(hi)
            with | _ ->
                do Console.Error.WriteLine($"Invalid range format '{range}'"); exit -1
                failwith range

    // derive parameters
    let wastedLines = (if title.Length > 0 then 2 else 0) + (if stats then 2 else 0)
    let m = height - wastedLines
    let worstCaseFloatCrap = 7 // sign + dot + e + eSign + 3 exponent digits
    let numericWidth = digits + worstCaseFloatCrap
    let n = width - numericWidth - 1 // 1 = space

    // escape weird corners of the parameter space
    if m < 2 then do
        Console.Error.WriteLine $"{width} x {height} plot region is not tall enough"
        exit -1
    if n < 2 then do
        Console.Error.WriteLine $"{width} x {height} plot region is not wide enough"
        exit -1

    // prepare format strings
    let clearWidth = String.replicate width " "
    let headerLine = if title.Length > 0 then $"{clearWidth}\r    {title}\n{clearWidth}\n" else ""
    let statsFormat = $"\n{clearWidth}\n{clearWidth}\r    now={{0:T}} avg={{1:g{max 3 digits}}} std={{2:g{max 3 digits}}} NaN={{3:d}}"
    let numberToString (width: int) (significantDigits: int) (x: float) =
        String.Format($"{{0,{width}:g{significantDigits}}}", x)
    let makeLabel (x: float) =
        let sign = if x < 0 then 1 else 0
        let dot = if round x <> x then 1 else 0
        let leadingZeros =
            String.Format("{0:g}", x)
            |> Seq.takeWhile (fun c -> not (Char.IsNumber c) || c = '0')
            |> Seq.filter (fun c -> c = '0')
            |> Seq.length
        let nonSignificant = sign + dot + leadingZeros
        let maxPrecision = numberToString numericWidth (numericWidth - nonSignificant) x
        if maxPrecision.Length > numericWidth then
            let constrainedPrecision = numberToString numericWidth digits x
            constrainedPrecision + " "
        else
            maxPrecision + " "

    // allocate mutable buffers
    let data = RingBuffer.create n nan
    let labels = Array.create m ""

    // pre-loop setup
    let mutable nans = 0
    let mutable input = Console.In.ReadLine()
    do
        Console.Clear()
        Console.CancelKeyPress.Add (fun _ -> Console.Clear(); Console.CursorVisible <- true)
        Console.CursorVisible <- false

    while not (isNull input) do
        // parse new data point
        let y = match Double.TryParse(input) with true, x -> x | _ -> nan
        if Double.IsFinite y then
            RingBuffer.enqueue data y

            // get updated stats
            let timeseries = RingBuffer.toSeq data
            let _, min, max, avg, var = Seq.statistics timeseries
            let min, max = if range = "" then min, max else lowerBound, upperBound
            let std = sqrt var
            let now = DateTime.Now

            // prepare Y axis labels
            let yaxis i = Math.lerp (0.0, float m - 1.0) (min, max) (float i)
            for i = 0 to labels.Length - 1 do
                labels[i] <- makeLabel (yaxis i)

            // plot and redraw the chart
            let plot = Graf.plot (m, n) (min, max) color timeseries
            Console.SetCursorPosition(0, 0)
            Console.Out.Write(headerLine)
            Console.Out.Write(Plot.toString labels plot)
            if stats then Console.Out.Write(statsFormat, now, avg, std, nans)
        else
            nans <- nans + 1

        // block until next input
        input <- Console.In.ReadLine()

    // on end of stream, return the number of lines which failed to parse
    do Console.CursorVisible <- true
    nans
