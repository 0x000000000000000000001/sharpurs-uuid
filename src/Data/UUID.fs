// UUID generation and validation matching the `uuid` npm package.

let private uuidHex (bytes: byte[]) : string =
    let sb = System.Text.StringBuilder(36)
    for i = 0 to 15 do
        if i = 4 || i = 6 || i = 8 || i = 10 then sb.Append('-') |> ignore
        sb.Append(bytes.[i].ToString("x2", System.Globalization.CultureInfo.InvariantCulture)) |> ignore
    sb.ToString()

let private networkOrder (guid: System.Guid) : byte[] =
    // System.Guid stores the first three fields little-endian, while RFC 4122
    // hashes work on the network order.
    let bytes = guid.ToByteArray()
    let network = Array.zeroCreate 16
    network.[0] <- bytes.[3]
    network.[1] <- bytes.[2]
    network.[2] <- bytes.[1]
    network.[3] <- bytes.[0]
    network.[4] <- bytes.[5]
    network.[5] <- bytes.[4]
    network.[6] <- bytes.[7]
    network.[7] <- bytes.[6]
    Array.blit bytes 8 network 8 8
    network

let private nameBasedUuid (version: byte) (hash: byte[] -> byte[]) (name: string) (namespaceValue: string) : string =
    let namespaceBytes = networkOrder (System.Guid.Parse(namespaceValue))
    let nameBytes = System.Text.Encoding.UTF8.GetBytes(name)
    let digest = hash (Array.append namespaceBytes nameBytes)
    let uuidBytes = Array.zeroCreate 16
    Array.blit digest 0 uuidBytes 0 16
    uuidBytes.[6] <- (uuidBytes.[6] &&& 0x0Fuy) ||| (version <<< 4)
    uuidBytes.[8] <- (uuidBytes.[8] &&& 0x3Fuy) ||| 0x80uy
    uuidHex uuidBytes

let getUUIDImpl : obj =
    box (fun (_: obj) -> box (System.Guid.NewGuid().ToString("D")))

let validateV4UUID (value: obj) : obj =
    match value with
    | :? string as str ->
        box (
            System.Text.RegularExpressions.Regex.IsMatch(
                str,
                "^(?:[0-9a-f]{8}-[0-9a-f]{4}-[1-8][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}|00000000-0000-0000-0000-000000000000|ffffffff-ffff-ffff-ffff-ffffffffffff)$",
                System.Text.RegularExpressions.RegexOptions.IgnoreCase
            )
        )
    | _ -> box false

let getUUID3Impl (name: obj) (namespaceValue: obj) : obj =
    box (nameBasedUuid 3uy System.Security.Cryptography.MD5.HashData (unbox<string> name) (unbox<string> namespaceValue))

let getUUID5Impl (name: obj) (namespaceValue: obj) : obj =
    box (nameBasedUuid 5uy System.Security.Cryptography.SHA1.HashData (unbox<string> name) (unbox<string> namespaceValue))
