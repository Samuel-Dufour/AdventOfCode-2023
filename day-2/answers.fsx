open System.IO
open System
open System.Collections.Generic

let filePath = $@"{__SOURCE_DIRECTORY__}\inputs.txt"

type Color =
    | Red
    | Blue
    | Green

    static member fromColorName colorName =
        match colorName with
        | "red" -> Red
        | "green" -> Green
        | "blue" -> Blue
        | _ -> failwith "Invalid color"

type ColorsCount = Dictionary<Color, int>

type Game =
    { id: int
      sets: ColorsCount list }

    member x.isGamePossible(colorsCount: ColorsCount) =
        let testSet (set: ColorsCount) =
            let allColorsInBag = set.Keys |> Seq.forall (fun k -> colorsCount.ContainsKey k)

            let enoughColorCubesInBag =
                set.Keys |> Seq.forall (fun k -> colorsCount[k] >= set[k])

            allColorsInBag && enoughColorCubesInBag

        x.sets |> Seq.forall testSet

    member x.requireColorsCount() =
        let folder (acc: Dictionary<Color, int>) (s:ColorsCount) =
                s.Keys
                |> Seq.iter (fun k ->
                    if acc.ContainsKey k |> not then
                        acc.Add(k, s[k])
                    elif acc[k] < s[k] then
                        acc[k] <- s[k])
                acc
        
        x.sets
        |> List.fold folder (Dictionary<Color, int>())


module Game =
    let toId line =
        let digits =
            line
            |> Seq.skipWhile (Char.IsDigit >> not)
            |> Seq.takeWhile Char.IsDigit
            |> Seq.toArray

        String(digits) |> int

    let toDist (s: string) =
        match s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) with
        | [| n; color |] -> Color.fromColorName color, int n
        | _ -> failwith "Invalid Distribution"

    let toSet (set: string) =
        let colors = set.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
        colors |> Array.map toDist |> dict |> Dictionary

    let toSets (line: string) =
        let sets = line.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
        sets |> Array.map toSet |> Array.toList

    let toGame (line: string) =
        match line.Split([| ':' |], StringSplitOptions.RemoveEmptyEntries) with
        | [|idPart; setsPart|] ->
            let id = toId idPart
            { id = id; sets = toSets setsPart }
        | _ -> failwith "Invalid Game"

module FirstAnswer =
    let colorsBag = [Red, 12; Green, 13; Blue, 14 ] |> dict |> Dictionary

    let toResult () =
        filePath
        |> File.ReadLines
        |> Seq.map Game.toGame
        |> Seq.where (fun g -> g.isGamePossible colorsBag)|> Seq.map (_.id)
        |> Seq.sum

module SecondAnswer =
    let colorsCountToValues (colorsCount: ColorsCount) =
        colorsCount.Keys |> Seq.map (fun k -> colorsCount[k]) |> Seq.reduce (*)

    let toResult () =
        filePath
        |> File.ReadLines
        |> Seq.map Game.toGame
        |> Seq.map (fun g -> g.requireColorsCount ())
        |> Seq.map colorsCountToValues
        |> Seq.sum

FirstAnswer.toResult()
SecondAnswer.toResult()