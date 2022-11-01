namespace Graf

(*
    TL;DR? Just do:
    ```
        timeseries // seq<float>
        |> Graf.plot (m,n) color
        |> Plot.toString labels
        |> System.Console.Out.WriteLine
    ```

    Given some input timeseries data:
        min, max = min(data), max(data)
        m = height - title - stats
        n = width - labelSize
        yaxis(i) = lerp (0, m-1) (min, max) i

    We'll plot a width*height TUI chart like:
            < plot title >

        <--- labels ---><-- n -->
        m-1 -> max      ┤     ┌─┐
        ...    ...      ┤     │ │
        i   -> yaxis(i) ┤┌──┐ │ └
        ...    ...      ┤│  │ │
        0   -> min      ┼┘  └─┘

            < stats: avg, std, nan >

    Using glyphs from code page 437 (Unicoded):
        │ u2502    ┤ u2524    ┐ u2510
        └ u2514    ┴ u2534    ┬ 252C
        ├ u251C    ─ u2500    ┼ u253C
        ┘ u2518    ┌ u250C    <space>
*)

open System


/// Encodes a "four-segment display" with UP, DOWN, LEFT, RIGHT and combinations thereof.
type FourSegmentDisplay = byte

module FourSegmentDisplay =
    [<Literal>]
    let EMPTY : FourSegmentDisplay = 0b0000uy

    [<Literal>]
    let LEFT  : FourSegmentDisplay = 0b0001uy

    [<Literal>]
    let UP    : FourSegmentDisplay = 0b0010uy

    [<Literal>]
    let RIGHT : FourSegmentDisplay = 0b0100uy

    [<Literal>]
    let DOWN  : FourSegmentDisplay = 0b1000uy

    let internal glyphs = Map [|
        (EMPTY,                    " ")
        (UP + DOWN,                "│")
        (LEFT + UP + DOWN,         "┤")
        (LEFT + DOWN,              "┐")
        (UP + RIGHT,               "└")
        (UP + RIGHT + DOWN,        "├")
        (LEFT + RIGHT,             "─")
        (LEFT + UP + RIGHT + DOWN, "┼")
        (LEFT + UP,                "┘")
        (RIGHT + DOWN,             "┌")
        (LEFT + UP + RIGHT,        "┴")
        (LEFT + RIGHT + DOWN,      "┬")
    |]

open FourSegmentDisplay


/// Encodes a 3-bit RGB (additive) color space for standard ANSI terminal colors.
type AnsiColor = byte

// 3 LSBs in ANSI color codes are defined such that C=G+B, M=R+B, Y=R+G, nice!
module AnsiColor =
    [<Literal>]
    let BLACK   : AnsiColor = 0uy

    [<Literal>]
    let RED     : AnsiColor = 1uy

    [<Literal>]
    let GREEN   : AnsiColor = 2uy

    [<Literal>]
    let YELLOW  : AnsiColor = 3uy

    [<Literal>]
    let BLUE    : AnsiColor = 4uy

    [<Literal>]
    let MAGENTA : AnsiColor = 5uy

    [<Literal>]
    let CYAN    : AnsiColor = 6uy

    [<Literal>]
    let WHITE   : AnsiColor = 7uy

    // ok, this is not a 3-bit color space after all
    let DEFAULT : AnsiColor = 225uy // = (byte Byte.MaxValue) - 30uy

    let internal code color =
        if color = DEFAULT then 0uy else 30uy + min WHITE color

    let toString color =
        let ansi = code color
        $"\x1b[{ansi}m"

open AnsiColor


/// Represents the content of a specific point in a plot spec.
[<Struct>]
type Point = {
    Segment: FourSegmentDisplay
    Color: AnsiColor
} with
    static member Blank =
        { Segment = EMPTY; Color = DEFAULT }

    override this.ToString() =
        let segment = min (LEFT + UP + RIGHT + DOWN) this.Segment
        let color = AnsiColor.toString this.Color
        let reset = AnsiColor.toString AnsiColor.DEFAULT
        match Map.tryFind segment FourSegmentDisplay.glyphs with
        | Some glyph -> color + glyph + reset
        | None -> failwith "single-segment points have no known mapping to CP437"

    static member (+) (lhs, rhs) =
        let diff = lhs.Color <> rhs.Color
        let mixed =
            let lhs = if lhs.Color = AnsiColor.DEFAULT then 0uy else lhs.Color
            let rhs = if rhs.Color = AnsiColor.DEFAULT then 0uy else rhs.Color
            min WHITE (lhs + rhs) // saturating addition
        { Segment = lhs.Segment ||| rhs.Segment
          Color = if diff then mixed else lhs.Color }

module Point =
    let withColor color point =
        { point with Color = color }

[<AutoOpen>]
module PointSugar =
    let point segment = {
        Segment = segment
        Color = AnsiColor.DEFAULT
    }


/// Fixed-capacity circular buffer for timeseries data.
type StatsQueue = private {
    Buffer: float[]
    mutable Front: int
    mutable Back: int
    mutable Sum: float
    mutable SquaredSum: float
    mutable Dropped: int
} with
    member private this.toSeq() = seq {
        let mutable i = this.Front
        let mutable stop = false
        while Double.IsFinite this.Buffer[i] && not stop do
            yield this.Buffer[i]
            i <- (i + 1) % this.Buffer.Length
            if i = this.Back then stop <- true
    }

    override this.ToString() =
        "[|" + (this.toSeq() |> Seq.map string |> String.concat "; ") + "|]"

// NOTE: we use NaNs to mark tombstones
[<RequireQualifiedAccess>]
module StatsQueue =
    let make capacity =
        if capacity <= 0 then failwith "queue capacity must be strictly positive"
        { Buffer = Array.create capacity nan; Front = 0; Back = 0
          Sum = 0.0; SquaredSum = 0.0; Dropped = 0 }

    let dequeue q =
        let x = q.Buffer[q.Front]
        if Double.IsFinite x then
            q.Buffer[q.Front] <- nan
            q.Front <- (q.Front + 1) % q.Buffer.Length
            q.Sum <- q.Sum - x
            q.SquaredSum <- q.SquaredSum - (x * x)
        x

    let enqueue q x =
        if Double.IsFinite q.Buffer[q.Back] then dequeue q |> ignore
        if Double.IsFinite x then
            q.Buffer[q.Back] <- x
            q.Back <- (q.Back + 1) % q.Buffer.Length
            q.Sum <- q.Sum + x
            q.SquaredSum <- q.SquaredSum + (x * x)
        else
            q.Dropped <- q.Dropped + 1

    let toSeq q = seq {
        let mutable i = q.Front
        let mutable stop = false
        while Double.IsFinite q.Buffer[i] && not stop do
            yield q.Buffer[i]
            i <- (i + 1) % q.Buffer.Length
            if i = q.Back then stop <- true
    }

    let length q =
        toSeq q |> Seq.length

    let sum q =
        q.Sum

    let min q =
        toSeq q |> Seq.min

    let max q =
        toSeq q |> Seq.max

    let avg q =
        let n = length q |> float
        q.Sum / n

    let var q =
        let n = length q |> float
        (q.SquaredSum - (q.Sum * q.Sum / n )) / (n - 1.0)

    let std q =
        sqrt (var q)

    let dropped q =
        q.Dropped


module Math =
    /// Linearly interpolates across two arbitrary ranges.
    let lerp (inMin, inMax) (outMin, outMax) x : float =
            (x - inMin) * (outMax - outMin) / (inMax - inMin) + outMin


/// Represents a (mutable) plot specification.
type Plot = private {
    Data: Point[]
    Rows: int
    Cols: int
} with
    override this.ToString() =
        this.Data
        |> Seq.map string
        |> Seq.chunkBySize this.Cols
        |> Seq.map (String.concat "")
        |> Seq.rev
        |> String.concat "\n"

[<RequireQualifiedAccess>]
module Plot =
    let create rows cols =
        let data = Array.create (rows * cols) (point EMPTY)
        { Data = data; Rows = rows; Cols = cols }

    let size plot =
        plot.Rows, plot.Cols

    let get plot (i, j) =
        plot.Data[i*plot.Cols + j]

    let set plot (i, j) x =
        plot.Data[i*plot.Cols + j] <- x

    let reset plot =
        Array.fill plot.Data 0 (plot.Rows * plot.Cols) (point EMPTY)

    let toString labels plot =
        plot.Data
        |> Seq.map string
        |> Seq.chunkBySize plot.Cols
        |> Seq.map (String.concat "")
        |> Seq.mapi (fun i row -> (Seq.item i labels) + row)
        |> Seq.rev
        |> String.concat "\n"

    let make (m, n) color data =
        let (+=) (plot, (i, j)) delta =
            set plot (i, j) <| (get plot (i, j)) + delta

        // compute range
        let min = Seq.min data
        let max = Seq.max data
        let min', max' =
            if min <> max then min, max else min - 1.0, max + 1.0

        // cache quantization results for each point
        let quantize y =
            if Double.IsFinite y then
                Math.lerp (min', max') (0.0, float m - 1.0) y
                |> round |> int
            else
                -1
        let buckets = Array.create n -1
        do data |> Seq.iteri (fun i y -> buckets[i] <- quantize y)

        // clear (initialize) the plot and draw Y axis
        let plot = create m n
        for i = 0 to m - 1 do set plot (i, 0) <| point (LEFT + UP + DOWN)

        // rasterize timeseries
        for rasterHeight = 0 to m - 1 do
            for x = 0 to n - 1 do
                // if there's a data point here, the RIGHT segment MUST be drawn
                let pointHeight = buckets[x]
                if pointHeight >= 0 then
                    let mutable y = if rasterHeight = pointHeight then RIGHT else EMPTY

                    // connector segments are drawn based on the current derivative
                    // and raster height with respect to the previous data point
                    if x > 0 then
                        let previous = buckets[x - 1]
                        assert Double.IsFinite previous
                        if previous = pointHeight && rasterHeight = pointHeight then
                            y <- y + LEFT
                        elif previous > pointHeight then
                            if rasterHeight = previous then y <- y + LEFT
                            if previous >= rasterHeight && rasterHeight > pointHeight then y <- y + DOWN
                            if previous > rasterHeight && rasterHeight >= pointHeight then y <- y + UP
                        elif previous < pointHeight then
                            if rasterHeight = previous then y <- y + LEFT
                            if previous <= rasterHeight && rasterHeight < pointHeight then y <- y + UP
                            if previous < rasterHeight && rasterHeight <= pointHeight then y <- y + DOWN

                    if y <> EMPTY then
                        (plot, (rasterHeight, x)) += (point y |> Point.withColor color)

        plot

[<AutoOpen>]
module Graf =
    let plot (rows, cols) color data =
        Plot.make (rows, cols) color data
