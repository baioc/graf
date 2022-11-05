// @baioc: I usually dislike per-file licenses, but putting it here lets people
// use this as a single-file library which can simply be copied into a project

(*
zlib License

Copyright (c) 2022 Gabriel B. Sant'Anna

This software is provided 'as-is', without any express or implied
warranty.  In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.

SPDX-License-Identifier: Zlib
*)

namespace Handmade.Collections.Generic


/// Fixed-capacity circular buffer / queue.
type RingBuffer<'T> = private {
    Buffer: 'T[]
    mutable Front: int
    mutable Back: int
    mutable Empty: bool
} with
    member this.GetEnumerator() =
        if this.Empty then
            Seq.empty
        else
            seq {
                let mutable i = this.Front
                let mutable stop = false
                while not stop do
                    yield this.Buffer[i]
                    i <- (i + 1) % this.Buffer.Length
                    if i = this.Back then stop <- true
            }

    override this.ToString() =
        "[|" + (this.GetEnumerator() |> Seq.map string |> String.concat "; ") + "|]"

    member this.Capacity =
        this.Buffer.Length

    member this.Count =
        if this.Empty then
            0
        elif this.Front < this.Back then
            this.Back - this.Front
        else
            let frontToWrap = this.Buffer.Length - this.Front
            let wrapToBack = this.Back
            frontToWrap + wrapToBack

    member this.Dequeue() =
        if this.Empty then failwith "cannot dequeue from an empty buffer"
        let x = this.Buffer[this.Front]
        this.Front <- (this.Front + 1) % this.Buffer.Length
        if this.Front = this.Back then this.Empty <- true
        x

    member this.Enqueue x =
        if this.Back = this.Front && not this.Empty then this.Dequeue() |> ignore
        this.Empty <- false
        this.Buffer[this.Back] <- x
        this.Back <- (this.Back + 1) % this.Buffer.Length


module RingBuffer =
    let create capacity value =
        if capacity <= 0 then failwith "buffer capacity must be strictly positive"
        { Buffer = Array.create capacity value; Front = 0; Back = 0; Empty = true }

    let capacity (q: RingBuffer<_>) =
        q.Capacity

    let length (q: RingBuffer<_>) =
        q.Count

    let dequeue (q: RingBuffer<_>) =
        q.Dequeue()

    let enqueue (q: RingBuffer<_>) x =
        q.Enqueue x

    let toSeq (q: RingBuffer<_>) =
        q.GetEnumerator()
