module Control.Extend

open System

let arrayExtend (f: obj) (xs: obj) : obj =
    let arr = unbox<obj[]> xs
    let outArr = Array.zeroCreate arr.Length
    for i = 0 to arr.Length - 1 do
        let slice = Array.zeroCreate (arr.Length - i)
        Array.Copy(arr, i, slice, 0, arr.Length - i)
        outArr.[i] <- sharpurs_apply f (slice :> obj)
    outArr :> obj
