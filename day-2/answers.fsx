open System.IO
open System

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

type ColorsCount = Map<Color, int>

type Game = { id: int; sets: ColorsCount list }

module Game =
    let requireColorsCount game =
        let folder acc s =
            let folder acc k =
                match Map.tryFind k acc, Map.find k s with
                | Some e, e' when e > e' -> acc
                | _, e -> acc.Add(k, e)

            s.Keys |> Seq.fold folder acc

        game.sets |> Seq.fold folder Map.empty<Color, int>

    let isGamePossible colorsCount game =
        let testSet set =
            let allColorsInBag =
                set |> Map.keys |> Seq.forall (fun k -> colorsCount |> Map.containsKey k)

            let enoughColorCubesInBag =
                set |> Map.keys |> Seq.forall (fun k -> colorsCount |> Map.find k >= set[k])

            allColorsInBag && enoughColorCubesInBag

        game.sets |> Seq.forall testSet


    let toGame (line: string) =
        let toId line =
            let digits =
                line
                |> Seq.skipWhile (Char.IsDigit >> not)
                |> Seq.takeWhile Char.IsDigit
                |> Seq.toArray

            String(digits) |> int

        let toSets (line: string) =
            let toSet (set: string) =
                let toDist (s: string) =
                    match s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) with
                    | [| n; color |] -> Color.fromColorName color, int n
                    | _ -> failwith "Invalid Distribution"

                set.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map toDist
                |> Map.ofArray

            line.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map toSet
            |> Array.toList

        match line.Split([| ':' |], StringSplitOptions.RemoveEmptyEntries) with
        | [| idPart; setsPart |] ->
            { id = toId idPart
              sets = toSets setsPart }
        | _ -> failwith "Invalid Game"

module FirstAnswer =
    let colorsBag = [ Red, 12; Green, 13; Blue, 14 ] |> Map.ofList
    let gameIsPossible = Game.isGamePossible colorsBag

    let toResult () =
        filePath
        |> File.ReadLines
        |> Seq.map Game.toGame
        |> Seq.where gameIsPossible
        |> Seq.map (fun x -> x.id)
        |> Seq.sum

module SecondAnswer =
    let colorsCountToValues = Map.values >> Seq.reduce (*)

    let toResult () =
        filePath
        |> File.ReadLines
        |> Seq.map (Game.toGame >> Game.requireColorsCount >> colorsCountToValues)
        |> Seq.sum

FirstAnswer.toResult ()
SecondAnswer.toResult ()
