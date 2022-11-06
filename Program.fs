open System

open Handmade.Collections.Generic
open Graf


module Seq =
    /// Returns element count, min, max, average and (sample) variance of a given seq.
    let statistics seq =
        let init = (0, Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0.0)
        let acc (n, m, M, s, ss) x = (n + 1, min m x, max M x, s + x, ss + x*x)
        let len, min, max, sum, squaredSum = seq |> Seq.filter (not << Double.IsNaN) |> Seq.fold acc init
        let n = float len
        let avg = sum / n
        let var = (squaredSum - (sum * sum / n )) / (n - 1.0)
        len, min, max, avg, var

let fatal (msg: string) =
    do Console.Error.WriteLine(msg)
    exit -1


let version = "0.3.3"
// TODO: wishlist for v1.0
// - permissive mode (ignore bad lines)
// - batch mode (only draw once)
// - write TSV/CSV to stderr
// - delimiter option

let usage = $"\
Usage: graf [OPTION]...
    Plot discretized line charts in your terminal.

Options:
    -h, --help           Print this help message and exit the program.
    -f, --file FILE      If FILE is - or not specified, read from stdin.
    -n, --lines N        Plot N <= 8 parallel lines. Default is inferred from 1st input.
    -t, --title TITLE    Display TITLE on top of the plotted chart.
    -c, --color COLOR    Color to plot the line in. See options below.
    -s, --stats          Show statistics, which are hidden by default.
    -r, --range MIN:MAX  Fix plot bounds instead of choosing them dynamically.
    -d, --digits DIGITS  Ensure at least DIGITS significant digits are printed.
    -W, --width WIDTH    Maximum TUI width. Defaults to terminal width.
    -H, --height HEIGHT  Maximum TUI height. Defaults to terminal height.

Notes:
    - A single quantization range is used for the entire chart, so make sure
    timeseries are similarly scaled when there are more than one.
    - When the chart includes multiple lines, a default title is added in order
    to help disambiguate them; furthermore, each timeseries is colored differently.
    - Options '--title' and '--color' can be specified multiple times, in which
    case they will be applied to each timeseries in a corresponding position.

Colors:
    \x1b[30m k, black \x1b[0m \t \x1b[31m r, red     \x1b[0m \t \x1b[32m g, green \x1b[0m \t \x1b[33m y, yellow \x1b[0m
    \x1b[34m b, blue  \x1b[0m \t \x1b[35m m, magenta \x1b[0m \t \x1b[36m c, cyan  \x1b[0m \t \x1b[37m w, white  \x1b[0m

Links:
    Project page  https://github.com/baioc/graf
    Bug tracker   https://github.com/baioc/graf/issues

graf v{version}
Copyright (c) 2022 Gabriel B. Sant'Anna"


type Options = {
    Help: bool
    File: string
    Lines: int
    Titles: seq<string>
    Colors: seq<string>
    Stats: bool
    Range: string
    Digits: int
    Width: int
    Height: int
}

let defaultOptions = {
    Help = false
    File = "-"
    Lines = 0
    Titles = []
    Colors = []
    Stats = false
    Range = ""
    Digits = 1
    Width = Console.BufferWidth
    Height = Console.BufferHeight
}

let defaultColors = [|
    AnsiColor.RED
    AnsiColor.GREEN
    AnsiColor.BLUE
    AnsiColor.MAGENTA
    AnsiColor.YELLOW
    AnsiColor.CYAN
    AnsiColor.WHITE
    AnsiColor.BLACK
|]

// parse command line
let rec parseCLI seen opts args =
    let set (arg: string) =
        if Set.contains arg seen then
            do Console.Error.WriteLine($"Warning: --{arg} option was set more than once")
            seen
        else
            Set.add arg seen

    let addColor c =
        // normalization
        let color = if c = "black" then "k" else c
        let color = if color.Length > 1 then color.Substring(0, 1) else color
        // deduplication
        if Seq.contains color opts.Colors then
            do Console.Error.WriteLine($"Warning: color '{c}' was used more than once")
            opts.Colors
        else
            Seq.append opts.Colors [color]

    match args with
    | [] -> opts

    | "-h"::rest | "--help"::rest ->
        parseCLI (set "help") { opts with Help = true } rest

    | "-f"::file::rest | "--file"::file::rest ->
        parseCLI (set "file") { opts with File = file } rest

    | "-n"::lines::rest | "--lines"::lines::rest ->
        parseCLI (set "lines") { opts with Lines = int lines } rest

    | "-t"::title::rest | "--title"::title::rest ->
        parseCLI seen { opts with Titles = Seq.append opts.Titles [title] } rest

    | "-c"::color::rest | "--color"::color::rest ->
        parseCLI seen { opts with Colors = addColor color } rest

    | "-s"::rest | "--stats"::rest ->
        parseCLI (set "stats") { opts with Stats = true } rest

    | "-r"::range::rest | "--range"::range::rest ->
        parseCLI (set "range") { opts with Range = range } rest

    | "-d"::digits::rest | "--digits"::digits::rest ->
        parseCLI (set "digits") { opts with Digits = int digits } rest

    | "-W"::width::rest | "--width"::width::rest ->
        parseCLI (set "width") { opts with Width = int width } rest

    | "-H"::height::rest | "--height"::height::rest ->
        parseCLI (set "height") { opts with Height = int height } rest

    | unexpected ->
        let sp = " "
        failwith $"unexpected option or missing argument at '{String.concat sp args}'"


let runWith multi (titles: string[]) (colors: AnsiColor[]) showStats (lowerBound, upperBound) digits width height =
        // infer parameters from 1st input line
        let mutable input = Console.In.ReadLine()
        if isNull input then exit 0
        let multi = if multi > 0 then multi else input.Trim().Split().Length
        if multi > 8 then fatal $"Error: at most 8 lines can be plotted in a single chart"
        assert (multi >= 1 && multi <= 8)

        // derived parameters
        let colors =
            if colors.Length > 0 then colors
            elif multi = 1 then [| AnsiColor.DEFAULT |]
            else defaultColors
        assert (colors.Length >= multi)
        let title =
            if multi = 1 && titles.Length > 0 then
                titles[0]
            elif multi = 1 && titles.Length = 0 then
                ""
            else
                Array.init multi (fun i -> if i < titles.Length then titles[i] else $"%%{i+1}")
                |> Seq.mapi (fun i t -> AnsiColor.colorize colors[i] t)
                |> String.concat "\t"
        let m = height - (if title.Length > 0 then 2 else 0) - (if showStats then 1 + multi else 0) - 1
        let worstCaseFloatCrap = 7 // sign + dot + e + eSign + 3 exponent digits
        let numericWidth = digits + worstCaseFloatCrap
        let n = width - numericWidth - 1 // 1 = space

        // escape weird corners of the parameter space
        if m < 2 then do fatal $"Error: {width} x {height} plot region is not tall enough"
        if n < 2 then do fatal $"Error: {width} x {height} plot region is not wide enough"

        // prepare format strings
        let clearWidth = String.replicate width " "
        let headerFormat = if title.Length > 0 then $"{clearWidth}\r[{{0:T}}]\t{title}\n{clearWidth}\n" else ""
        let statsFormat =
            let g = max 3 digits
            $"{clearWidth}\r    min={{0:g{g}}} max={{1:g{g}}} avg={{2:g{g}}} std={{3:g{g}}} nan={{4:d}}"
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
        let timeseries = Array.init multi (fun i -> RingBuffer.create n nan)
        let nans = Array.create multi 0
        let chart = Chart.create m n
        let labels = Array.create m ""

        // pre-loop setup
        do
            Console.Clear()
            Console.CancelKeyPress.Add (fun _ -> Console.Clear(); Console.CursorVisible <- true)
            Console.CursorVisible <- false

        // parse new data points(s)
        while not (isNull input) do
            let strs = input.Trim().Split()
            if strs.Length <> multi then
                Console.CursorVisible <- true
                fatal $"Error: expected {multi} whitespace-separated values, but found '{input}'"
            strs |> Seq.iteri (fun i str ->
                let y = match Double.TryParse(str) with true, y -> y | _ -> nan
                RingBuffer.enqueue timeseries[i] y
                if not (Double.IsFinite y) then nans[i] <- nans[i] + 1
            )

            // compute global quantization range
            let timeseqs =  timeseries |> Seq.map RingBuffer.toSeq
            let min =
                if Double.IsFinite lowerBound then lowerBound
                else timeseqs |> Seq.map Seq.min |> Seq.min
            let max =
                if Double.IsFinite upperBound then upperBound
                else timeseqs |> Seq.map Seq.max |> Seq.max

            // clear the chart and (re)draw each line
            Chart.clear chart
            timeseqs |> Seq.iteri (fun i data ->
                ChartLine.ofSeq data
                |> ChartLine.withColor colors[i]
                |> ChartLine.withBounds (min, max)
                |> Chart.draw chart
            )

            // prepare Y axis labels
            let yaxis i = Math.lerp (0.0, float m - 1.0) (min, max) (float i)
            for i = 0 to labels.Length - 1 do
                labels[i] <- makeLabel (yaxis i)

            // render the chart as text
            Console.SetCursorPosition(0, 0)
            Console.Out.Write(headerFormat, DateTime.Now)
            Console.Out.WriteLine(chart.ToString(labels))
            if showStats then
                Console.Out.WriteLine(clearWidth)
                timeseqs |> Seq.iteri (fun i data ->
                    let _, min, max, avg, var = Seq.statistics data
                    let std = sqrt var
                    Console.Out.WriteLine(AnsiColor.colorize colors[i] statsFormat, min, max, avg, std, nans[i])
                )
            Console.Out.Flush()

            // if next input is not available, call the GC before blocking
            if Console.In.Peek() < 0 then GC.Collect()
            input <- Console.In.ReadLine()

        // on end of stream, return the number of values which failed to parse
        do Console.CursorVisible <- true
        Seq.sum nans


[<EntryPoint>]
let main argv =
    let opts =
        try
            parseCLI Set.empty defaultOptions (List.ofArray argv)
        with
            | ex -> fatal $"Error: invalid command line syntax; {ex.Message}\n\n{usage}"

    if opts.Help then
        do Console.Out.WriteLine(usage); exit 0

    if opts.File <> "-" then
        try
            do Console.SetIn(new IO.StreamReader(opts.File))
        with
            | _ -> fatal $"Error: could not open file '{opts.File}'"

    let parseColor str =
        let maybeColor =
            Map.tryFind str (Map.ofSeq [
                ("" , AnsiColor.DEFAULT)
                ("k", AnsiColor.BLACK); ("black", AnsiColor.BLACK)
                ("r", AnsiColor.RED); ("red", AnsiColor.RED)
                ("g", AnsiColor.GREEN); ("green", AnsiColor.GREEN)
                ("y", AnsiColor.YELLOW); ("yellow", AnsiColor.YELLOW)
                ("b", AnsiColor.BLUE); ("blue", AnsiColor.BLUE)
                ("m", AnsiColor.MAGENTA); ("magenta", AnsiColor.MAGENTA)
                ("c", AnsiColor.CYAN); ("cyan", AnsiColor.CYAN)
                ("w", AnsiColor.WHITE); ("white", AnsiColor.WHITE)
            ])
        if Option.isNone maybeColor then fatal $"Error: unknown color {str}"
        else maybeColor.Value

    let titles = Array.ofSeq opts.Titles

    let colors =
        if Seq.isEmpty opts.Colors then
            [||]
        else
            let parsed = opts.Colors |> Seq.map parseColor
            let exclude = Set.ofSeq parsed
            let added = defaultColors |> Seq.filter (fun c -> not (Set.contains c exclude))
            Seq.append parsed added |> Array.ofSeq

    let min, max =
        if opts.Range = "" then
            Double.NegativeInfinity, Double.PositiveInfinity
        else
            try
                let subs = opts.Range.Split(':')
                if subs.Length <> 2 then failwith opts.Range
                let lo, hi = subs[0], subs[1]
                Double.Parse(lo), Double.Parse(hi)
            with | _ ->
                fatal $"Error: invalid range format '{opts.Range}'"

    let enforce var pred n =
        if not (pred n) then fatal $"Error: parameter {var} ({n}) out of range"

    enforce "lines" (fun n -> n >= 0 && n <= 8) opts.Lines
    enforce "digits" (fun n -> n > 0) opts.Digits
    enforce "width" (fun n -> n > 0) opts.Width
    enforce "height" (fun n -> n > 0) opts.Height

    runWith opts.Lines titles colors opts.Stats (min, max) opts.Digits opts.Width opts.Height
