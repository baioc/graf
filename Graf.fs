// @baioc: I usually dislike per-file licenses, but putting it here lets people
// use this as a single-file library which can simply be copied into a project

(*
 * Copyright (c) 2022 Gabriel B. Sant'Anna
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the author nor the names of its contributors may
 *    be used to endorse or promote products derived from this software
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * SPDX-License-Identifier: BSD-3-Clause
*)

// Given some input timeseries data:
//     min, max = min(data), max(data)
//     m = height - title - stats
//     n = width - labelSize
//     yaxis(i) = lerp (0, m-1) (min, max) i
//
// We'll plot a width*height TUI chart like:
//         < header >
//     <--- labels ---><-- n -->
//     m-1 -> max      ┤     ┌─┐
//     ...    ...      ┤     │ │
//     i   -> yaxis(i) ┤┌──┐ │ └
//     ...    ...      ┤│  │ │
//     0   -> min      ┼┘  └─┘
//         < footer >
//
// Using codepage 437 glyphs (see Unicode "Box Drawing" block):
//     │ u2502    ┤ u2524    ┐ u2510
//     └ u2514    ┴ u2534    ┬ 252C
//     ├ u251C    ─ u2500    ┼ u253C
//     ┘ u2518    ┌ u250C    <space>

module Graf

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

open FourSegmentDisplay


/// Encodes a 3-bit color space for standard ANSI terminal colors.
type AnsiColor = byte

module AnsiColor =
    [<Literal>]
    let BLACK   : AnsiColor = 0b000uy

    [<Literal>]
    let RED     : AnsiColor = 0b001uy

    [<Literal>]
    let GREEN   : AnsiColor = 0b010uy

    [<Literal>]
    let YELLOW  : AnsiColor = 0b011uy

    [<Literal>]
    let BLUE    : AnsiColor = 0b100uy

    [<Literal>]
    let MAGENTA : AnsiColor = 0b101uy

    [<Literal>]
    let CYAN    : AnsiColor = 0b110uy

    [<Literal>]
    let WHITE   : AnsiColor = 0b111uy

    // ok, this is not just a 3-bit color space after all
    [<Literal>]
    let DEFAULT : AnsiColor = 225uy // = (byte Byte.MaxValue) - 30uy

    /// Converts a **conceptual** AnsiColor into an **actual** ANSI terminal color code.
    let code color =
        if color = DEFAULT then 0 else int (30uy + (WHITE &&& color))

    /// Wraps a stringifibale value in a given color, then switches to the default style.
    let wrap color thing =
        let str = string thing
        if str.Length = 0 then str
        else $"\x1b[{code color}m{str}\x1b[{code DEFAULT}m"

    /// Like `wrap`, but does nothing if the provided color is already DEFAULT.
    let colorize color str =
        if color = DEFAULT then string str else wrap color str

    // NICE: 3 LSBs in ANSI color codes are defined such that the following math works out.

    /// Mixes two colors together in additive (i.e. RGB model) fashion.
    let add lhs rhs =
        if lhs = DEFAULT then rhs
        elif rhs = DEFAULT then lhs
        else lhs ||| rhs

    /// Mixes two colors together in subtractive (i.e. CMY model) fashion.
    let sub lhs rhs =
        if lhs = DEFAULT then rhs
        elif rhs = DEFAULT then lhs
        else lhs &&& rhs

open AnsiColor


/// Represents the contents of a specific point in a chart.
type ChartPoint = struct
    val Segment: FourSegmentDisplay
    val Color: AnsiColor

    new(segment, color) = { Segment = segment; Color = color }
    new(segment) = { Segment = segment; Color = AnsiColor.DEFAULT }

    static member (+) (lhs: ChartPoint, rhs: ChartPoint) =
        let mix = add lhs.Color rhs.Color
        let ensureDifferent =
            if lhs.Color <> rhs.Color &&
               (mix = lhs.Color || mix = rhs.Color) &&
               lhs.Color <> AnsiColor.DEFAULT &&
               rhs.Color <> AnsiColor.DEFAULT
            then (lhs.Color + rhs.Color) % (WHITE + 1uy)
            else mix
        ChartPoint(lhs.Segment ||| rhs.Segment, ensureDifferent)

    static member private glyphs = [|
        (*           EMPTY          *) ' '
        (*                     LEFT *) 'Χ' // <- how NaNs render
        (*                UP        *) '?'
        (*                UP + LEFT *) '┘'
        (*        RIGHT             *) '?'
        (*        RIGHT +      LEFT *) '─'
        (*        RIGHT + UP        *) '└'
        (*        RIGHT + UP + LEFT *) '┴'
        (* DOWN                     *) '?'
        (* DOWN +              LEFT *) '┐'
        (* DOWN +         UP        *) '│'
        (* DOWN +         UP + LEFT *) '┤'
        (* DOWN + RIGHT             *) '┌'
        (* DOWN + RIGHT   +    LEFT *) '┬'
        (* DOWN + RIGHT + UP        *) '├'
        (* DOWN + RIGHT + UP + LEFT *) '┼'
    |]

    override this.ToString() =
        let segment = (DOWN + RIGHT + UP + LEFT) &&& this.Segment
        colorize this.Color ChartPoint.glyphs[int segment]
end


/// Serves as input to be drawn in charts.
[<Struct>]
type ChartLine = private {
    Data: seq<float>
    Color: AnsiColor
    Min: float; Max: float
}

module ChartLine =
    let ofSeq data =
        { Data = data
          Min = Double.NegativeInfinity; Max = Double.PositiveInfinity
          Color = AnsiColor.DEFAULT }

    let withColor color line =
        { line with Color = color }

    let withBounds (min, max) line =
        { line with Min = min; Max = max }


module Math =
    /// Linearly interpolates a value across two arbitrary ranges.
    let lerp (inMin, inMax) (outMin, outMax) x : float =
            (x - inMin) * (outMax - outMin) / (inMax - inMin) + outMin


/// Represents a (mutable) specification of a TUI chart.
[<Struct>]
type Chart = private {
    Data: ChartPoint[]
    Rows: int16
    Cols: int16
} with
    /// Plots a chart to text, but with a prefix and suffix on each line (sorted from lowest to highest on the Y axis).
    member this.ToString(prefixes, suffixes) =
        let rows = int this.Rows
        let empty = Seq.init rows (fun _ -> "")
        let prefixes = prefixes |> Seq.truncate rows |> (fun seq -> Seq.append seq empty)
        let suffixes = suffixes |> Seq.truncate rows |> (fun seq -> Seq.append seq empty)
        this.Data
        |> Seq.map string
        |> Seq.chunkBySize (int this.Cols)
        |> Seq.map (String.concat "")
        |> (fun lines -> Seq.zip3 prefixes lines suffixes)
        |> Seq.map (fun (pre, row, suf) -> pre + row + suf)
        |> Seq.rev
        |> String.concat "\n"

    member this.ToString(prefixes) =
        this.ToString(prefixes, Seq.empty)

    override this.ToString() =
        this.ToString(Seq.empty, Seq.empty)

module Chart =
    let clear chart =
        chart.Data
        |> Array.iteri (fun ij _ ->
            chart.Data[ij] <- if ij % (int chart.Cols) = 0 then ChartPoint (DOWN + UP + LEFT)
                                else ChartPoint EMPTY)

    /// Create an empty chart, with fixed dimensions.
    let create rows cols =
        let chart =
            { Data = Array.create (rows * cols) (ChartPoint EMPTY)
              Rows = int16 rows; Cols = int16 cols }
        do clear chart
        chart

    let size chart =
        int chart.Rows, int chart.Cols

    let get chart (i, j) =
        chart.Data[i*(int chart.Cols) + j]

    let set chart (i, j) x =
        do chart.Data[i*(int chart.Cols) + j] <- x

    let private draw' chart (line: ChartLine) (data: float[]) =
        // XXX: beware of NaNs (see https://github.com/dotnet/fsharp/issues/13207)
        let minNonNan s = seq { yield Double.PositiveInfinity; yield! s } |> Seq.min
        let maxNonNan s = seq { yield Double.NegativeInfinity; yield! s } |> Seq.max

        // get data range
        let rows, _ = size chart
        let min, max =
            let min' = if Double.IsFinite line.Min then line.Min else minNonNan data
            let max' = if Double.IsFinite line.Max then line.Max else maxNonNan data
            let min'' = if Double.IsFinite min' then min' else 0.0
            let max'' = if Double.IsFinite max' then max' else 0.0
            if min'' <> max'' then min'', max'' else min'' - 1.0, max'' + 1.0

        // cache quantization results for each point
        let quantize y = Math.lerp (min, max) (0.0, float rows - 1.0) y |> (round >> int)
        let buckets = Array.create data.Length -1
        do data |> Array.iteri (fun i y ->
            if not (Double.IsNaN y) then buckets[i] <- quantize y
            else buckets[i] <- (if i > 0 then buckets[i - 1] else rows / 2))

        // rasterize timeseries
        for rasterHeight = 0 to rows - 1 do
            for col = 0 to data.Length - 1 do
                // if there's a data point here, the RIGHT segment MUST be drawn
                let pointHeight = buckets[col]
                let isNan = Double.IsNaN data[col]
                let mutable d =
                    let here = rasterHeight = pointHeight
                    if not here then EMPTY
                    elif not isNan then RIGHT
                    else LEFT

                // connector segments are drawn based on the current derivative
                // and raster height with respect to the previous data point
                if col > 0 && not isNan then
                    let prevHeight = buckets[col - 1]
                    if prevHeight = pointHeight then
                        if prevHeight = rasterHeight then d <- d + LEFT
                    elif prevHeight > pointHeight then
                        if prevHeight = rasterHeight then d <- d + LEFT
                        if prevHeight >= rasterHeight && rasterHeight > pointHeight then d <- d + DOWN
                        if prevHeight > rasterHeight && rasterHeight >= pointHeight then d <- d + UP
                    elif prevHeight < pointHeight then
                        if prevHeight = rasterHeight then d <- d + LEFT
                        if prevHeight <= rasterHeight && rasterHeight < pointHeight then d <- d + UP
                        if prevHeight < rasterHeight && rasterHeight <= pointHeight then d <- d + DOWN

                if d <> EMPTY then
                    let p = get chart (rasterHeight, col)
                    set chart (rasterHeight, col) <| p + ChartPoint(d, line.Color)

    /// Paints an individual line across the chart.
    let draw chart (line: ChartLine) =
        let _, cols = size chart
        let data = Seq.truncate cols line.Data |> Array.ofSeq
        if data.Length > 0 then draw' chart line data // skip empty lines
