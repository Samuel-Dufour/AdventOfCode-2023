open System.IO
open System.Text.RegularExpressions

let filePath = $@"{__SOURCE_DIRECTORY__}\test-input.txt"
// let filePath = $@"{__SOURCE_DIRECTORY__}\inputs.txt"

type Time = Time of int
type Distance = Distance of int

type Race =
    { Time: Time
      Distance: Distance }

    static member compute({ Time = Time t; Distance = Distance d }) = 
        let speed = 1
        let traveledDistance  raceTime waitTime = 
            let travelTime = raceTime - waitTime
            waitTime * speed * travelTime
        let traveledDistance = traveledDistance t

        [|
            for ms in 0..t do
                traveledDistance ms
        |]
        |> Array.where (fun td -> td > d)
        |> Array.length

let (|ParseRegex|) pattern s =
    let m = Regex.Matches(s, pattern)
    let ss = Regex.IsMatch(s, pattern)

    if ss then
        m |> Seq.map (fun s -> s.Value |> int) |> Seq.toArray |> Some
    else
        None

let (|LabeledInts|_|) (label: string) (s: string) =
    match s.Split(label) with
    | [| _; ParseRegex "(\d+)" times |] -> times |> Some
    | _ -> None

let (|Times|_|) (s: string) =
    match s with
    | LabeledInts "Time:" times -> times
    | _ -> None

let (|Distances|_|) (s: string) =
    match s with
    | LabeledInts "Distance:" times -> times
    | _ -> None


let toRaces lines =
    let tryToTimes (line: string) =
        match line with
        | Times times -> times |> Array.map Time |> Some
        | _ -> None

    let tryToDistances (line: string) =

        match line with
        | Distances times -> times |> Array.map Distance |> Some
        | _ -> None

    let times = lines |> Array.map tryToTimes |> Array.choose id |> Array.collect id

    let distances =
        lines |> Array.map tryToDistances |> Array.choose id |> Array.collect id

    Array.zip times distances
    |> Array.map (fun (t, d) -> { Time = t; Distance = d })

filePath |> File.ReadAllLines |> toRaces //|> Array.map Race.compute|> Array.reduce (*)
