open System
open System.Globalization
open Graf

let version = "0.1.b"

[<EntryPoint>]
let main argv =
    do CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture

    // parse CLI options
    let title = ""
    let color = AnsiColor.BLUE
    let stats = false
    let digits = 6
    let width = Console.BufferWidth
    let height = Console.BufferHeight

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
    let statsFormat = $"\n{clearWidth}\n{clearWidth}\r    now={{0:T}} avg={{1:g{max 3 digits}}} std={{2:g{max 3 digits}}} nans={{3:d}}"
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
    let data = StatsQueue.make n
    let labels = Array.create m ""

    // pre-loop setup
    let mutable input = Console.In.ReadLine()
    do
        Console.Clear()
        Console.CancelKeyPress.Add (fun _ -> Console.Clear(); Console.CursorVisible <- true)
        Console.CursorVisible <- false

    while not (isNull input) do
        // parse new data point
        let y = match Double.TryParse(input) with true, x -> x | _ -> nan
        StatsQueue.enqueue data y

        // get updated stats
        if StatsQueue.length data > 0 then
            let min = StatsQueue.min data
            let max = StatsQueue.max data
            let now = DateTime.Now
            let avg = StatsQueue.avg data
            let std = StatsQueue.std data
            let nans = StatsQueue.dropped data

            // prepare Y axis labels
            let yaxis i = Math.lerp (0.0, float m - 1.0) (min, max) (float i)
            for i = 0 to labels.Length - 1 do
                labels[i] <- makeLabel (yaxis i)

            // plot and redraw the chart
            let plot = Graf.plot (m, n) color (StatsQueue.toSeq data)
            do
                Console.SetCursorPosition(0, 0)
                Console.Out.Write(headerLine)
                Console.Out.Write(Plot.toString labels plot)
                if stats then Console.Out.Write(statsFormat, now, avg, std, nans)

        // block until next input
        input <- Console.In.ReadLine()

    // on end of stream, return the number of lines which failed to parse
    do Console.CursorVisible <- true
    StatsQueue.dropped data
