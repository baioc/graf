namespace Graf

(*
    Given some input timeseries data:
        min, max = min(data), max(data)
        m = height - title - stats
        n = width - labelSize
        yaxis(i) = lerp (0, m-1) (min, max) i

    We'll plot a width*height TUI graph like:
            < plot title >

        <--- labels ---><-- n -->
        m-1 -> max      ┤     ┌─┐
        ...    ...      ┤     │ │
        i   -> yaxis(i) ┤┌──┐ │ └
        ...    ...      ┤│  │ │
        0   -> min      ┼┘  └─┘

            < stats: avg, std, nan >

    Using glyphs from codepage 437 (Unicoded):
        │ u2502    ┤ u2524    ┐ u2510
        └ u2514    ┴ u2534    ┬ 252C
        ├ u251C    ─ u2500    ┼ u253C
        ┘ u2518    ┌ u250C    <space>
*)

open System


/// Encodes a "four-segment display" with UP, DOWN, LEFT, RIGHT or any combination of these.
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

open FourSegmentDisplay


/// Represents the content of a specific point in a plot spec.
[<Struct>]
type Point = {
    Segment: FourSegmentDisplay
} with
    override this.ToString() =
        Map [|
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
        |] |> Map.find this.Segment

    static member (+) (lhs, rhs) =
        { Segment = lhs.Segment ||| rhs.Segment }

    static member Empty =
        { Segment = EMPTY }

[<AutoOpen>]
module Point =
    let point segment =
        { Segment = segment }


/// Fixed-capacity circular buffer for floats, also tracking statistics.
type StatsQueue = private {
    Buffer: float[]
    mutable Front: int
    mutable Back: int
    mutable Sum: float
    mutable SquaredSum: float
    mutable Dropped: int
}

// NOTE: we use NaNs to mark tombstones
[<RequireQualifiedAccess>]
module StatsQueue =
    let make capacity =
        if capacity <= 0 then failwithf "queue capacity must be positive"
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
    /// Linearly interpolates a value across two arbitrary ranges.
    let lerp (inMin, inMax) (outMin, outMax) x : float =
            (x - inMin) * (outMax - outMin) / (inMax - inMin) + outMin


/// Represents a (mutable) graph plot specification.
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
        let data = Array.create (rows * cols) Point.Empty
        { Data = data; Rows = rows; Cols = cols }

    let size plot =
        plot.Rows, plot.Cols

    let get plot (i, j) =
        plot.Data[i*plot.Cols + j]

    let set plot (i, j) x =
        plot.Data[i*plot.Cols + j] <- x

    let reset plot =
        Array.fill plot.Data 0 (plot.Rows * plot.Cols) Point.Empty

    let toString plot labels =
        plot.Data
        |> Seq.map string
        |> Seq.chunkBySize plot.Cols
        |> Seq.map (String.concat "")
        |> Seq.mapi (fun i row -> (Seq.item i labels) + row)
        |> Seq.rev
        |> String.concat "\n"

    let private (+=) (plot, (i, j)) delta =
        set plot (i, j) <| (get plot (i, j)) + delta

    let make (m, n) data =
        // allocate buffers
        let buckets = Array.create n -1
        let plot = create m n

        // compute range
        let min' = Seq.min data
        let max' = Seq.max data

        // cache quantization results for each point
        let quantize y =
            if Double.IsFinite y then
                Math.lerp (min', max') (0.0, float m - 1.0) y
                |> round |> int
            else
                -1
        do data |> Seq.iteri (fun i y -> buckets[i] <- quantize y)

        // draw Y axis
        for i = 0 to m - 1 do
            set plot (i, 0) <| point (LEFT + UP + DOWN)

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

                    (plot, (rasterHeight, x)) += point y

        plot

[<AutoOpen>]
module Graf =
    let plot (rows, cols) data = Plot.make (rows, cols) data
