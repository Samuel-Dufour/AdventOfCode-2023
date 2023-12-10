open System.IO
open System.Text.RegularExpressions

// let filePath = $@"{__SOURCE_DIRECTORY__}\test-input.txt"
let filePath = $@"{__SOURCE_DIRECTORY__}\inputs.txt"

type Time = Time of int64
type Distance = Distance of int64

type Race =
    { Time: Time
      Distance: Distance }

    static member compute({ Time = Time t; Distance = Distance d }) = 
        let speed = 1L
        let traveledDistance  raceTime waitTime = 
            let travelTime = raceTime - waitTime
            waitTime * speed * travelTime
        let traveledDistance = traveledDistance t

        [|
            for ms in 0L..t do
                traveledDistance ms
        |]
        |> Array.where (fun td -> td > d)
        |> Array.length

let (|ParseRegex|) pattern s =
    let m = Regex.Matches(s, pattern)
    let ss = Regex.IsMatch(s, pattern)

    if ss then
        m |> Seq.map (fun s -> s.Value |> int64) |> Seq.toArray |> Some
    else
        None

let (|ParseRegex2|) pattern s =
    let m = Regex.Matches(s, pattern)
    let ss = Regex.IsMatch(s, pattern)

    if ss then
        m |> Seq.map (fun s -> s.Value) |> Seq.fold (fun acc d -> acc + d) "" |> int64 |> Some
    else
        None

let (|LabeledInts|_|) (label: string) (s: string) =
    match s.Split(label) with
    | [| _; ParseRegex "(\d+)" times |] -> times |> Some
    | _ -> None

let (|LabeledInt|_|) (label: string) (s: string) =
    match s.Split(label) with
    | [| _; ParseRegex2 "(\d+)" times |] -> times |> Some
    | _ -> None

let (|Times|_|) (s: string) =
    match s with
    | LabeledInt "Time:" times -> times
    | _ -> None

let (|Distances|_|) (s: string) =
    match s with
    | LabeledInt "Distance:" times -> times
    | _ -> None


let toRaces lines =
    let tryToTimes (line: string) =
        match line with
        | Times times -> times |> Time |> Some
        | _ -> None

    let tryToDistances (line: string) =

        match line with
        | Distances times -> times |> Distance |> Some
        | _ -> None

    let times = lines |> Array.map tryToTimes |> Array.choose id 

    let distances =
        lines |> Array.map tryToDistances |> Array.choose id 

    Array.zip times distances
    |> Array.map (fun (t, d) -> { Time = t; Distance = d })

filePath |> File.ReadAllLines |> toRaces |> Array.map Race.compute//|> Array.reduce (*)


