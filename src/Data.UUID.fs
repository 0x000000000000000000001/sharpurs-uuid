module Data.UUID

open System
open System.Security.Cryptography
open System.Text

let validateV4UUID (s: obj) : obj =
    let str = string s
    match Guid.TryParse(str) with
    | true, _ -> box true
    | _ -> box false

let getUUIDImpl (unitValue: obj) : obj =
    box (Guid.NewGuid().ToString().ToLowerInvariant())

let createHashUUID (version: byte) (hashAlg: HashAlgorithm) (name: string) (ns: string) : string =
    let nsBytes = Guid.Parse(ns).ToByteArray()
    if BitConverter.IsLittleEndian then
        Array.Reverse(nsBytes, 0, 4)
        Array.Reverse(nsBytes, 4, 2)
        Array.Reverse(nsBytes, 6, 2)
    
    let nameBytes = Encoding.UTF8.GetBytes(name)
    let buffer = Array.append nsBytes nameBytes
    let hash = hashAlg.ComputeHash(buffer)
    
    let res = Array.zeroCreate 16
    Array.Copy(hash, 0, res, 0, 16)
    
    res.[6] <- (res.[6] &&& 0x0Fuy) ||| version
    res.[8] <- (res.[8] &&& 0x3Fuy) ||| 0x80uy
    
    if BitConverter.IsLittleEndian then
        Array.Reverse(res, 0, 4)
        Array.Reverse(res, 4, 2)
        Array.Reverse(res, 6, 2)
        
    Guid(res).ToString().ToLowerInvariant()

let getUUID3Impl (str: obj) : obj =
    box (fun (ns: obj) ->
        use md5 = MD5.Create()
        box (createHashUUID 0x30uy md5 (string str) (string ns)))

let getUUID5Impl (str: obj) : obj =
    box (fun (ns: obj) ->
        use sha1 = SHA1.Create()
        box (createHashUUID 0x50uy sha1 (string str) (string ns)))
