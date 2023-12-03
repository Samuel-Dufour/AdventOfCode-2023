open System.IO
open System

let filePath = $@"{__SOURCE_DIRECTORY__}\test-input.txt"

let lines = File.ReadAllLines filePath
let columnsCount = lines[0].Length
let rowssCount = lines.Length

let board =
    Array2D.init columnsCount rowssCount (fun cidx ridx -> lines[cidx][ridx])

let digitCoords =
    board
    |> Array2D.mapi (fun ridx cidx c ->
        if Char.IsDigit c then
            Some({| ridx = ridx; cidx = cidx |})
        else
            None)

let digitSpans =
    [| for ridx in 0 .. Array2D.length1 digitCoords - 1 do
           digitCoords[ridx, *] |> Array.choose id |]
    |> Array.collect id
    |> Array.fold
        (fun acc coords ->
            match acc with
            | [] -> {| left = coords; right = coords |} :: acc
            | c :: tail when c.left.ridx = coords.ridx && c.right.cidx + 1 = coords.cidx ->
                {| c with right = coords |} :: tail
            | _ -> {| left = coords; right = coords |} :: acc)
        List.empty
    |> List.map (fun i ->
        {| i with
            number = String(board[i.left.ridx, i.left.cidx .. i.right.cidx]) |> int |})
    |> List.rev

let intervalsToCheck =
    digitSpans
    |> List.map (fun s ->
        let maxRidx = Array2D.length1 board - 1
        let maxCidx = Array2D.length2 board - 1

        let toCheckInterval

            (s:
                {| left: {| cidx: int; ridx: int |}
                   right: {| cidx: int; ridx: int |}
                   number: int |})
            =
            {| s with
                left =
                    {| cidx = max 0 (s.left.cidx - 1)
                       ridx = max 0 (s.left.ridx - 1) |}
                right =
                    {| cidx = min maxCidx (s.right.cidx + 1)
                       ridx = min maxRidx (s.right.ridx + 1) |}

            |}

        s |> toCheckInterval)


module FirstAnswer =
    let result () =
        intervalsToCheck
        |> List.map (fun i ->
            {| board = board[i.left.ridx .. i.right.ridx, i.left.cidx .. i.right.cidx]
               number = i.number |})
        |> List.map (fun i ->
            let charsCheck = i.board |> Array2D.map (fun c -> Char.IsDigit c |> not && c <> '.')

            {| number = i.number
               isOk =
                [| for ridx in 0 .. Array2D.length1 charsCheck - 1 do
                       charsCheck[ridx, *] |]
                |> Array.collect id
                |> Array.contains true |})
        |> List.where (fun i -> i.isOk)
        |> List.map (fun i -> i.number)
        |> List.sum

module SecondAnswer =
    let result () =
        intervalsToCheck
        |> List.map (fun i ->
            {| board = board[i.left.ridx .. i.right.ridx, i.left.cidx .. i.right.cidx]
               number = i.number
               left = i.left |})
        |> List.map (fun i ->
            // let charsCheck = i.board |> Array2D.map (fun c -> Char.IsDigit c |> not && c <> '.')
            let gearCheck =
                i.board
                |> Array2D.mapi (fun ridx cidx c ->
                    if c = '*' then
                        Some
                            {| ridx = i.left.ridx + ridx
                               cidx = i.left.cidx + cidx |}
                    else
                        None)

            let gear =
                [| for ridx in 0 .. Array2D.length1 gearCheck - 1 do
                       gearCheck[ridx, *] |]
                |> Array.collect id
                |> Array.choose id

            if Array.isEmpty gear then
                None
            else
                Some {| number = i.number; gear = gear[0] |}

        )
        |> List.choose id
        |> List.groupBy (fun l -> l.gear)
        |> List.filter (fun (_, l) -> l |> List.length = 2)
        |> List.map (fun (_, l) -> l |> List.map (fun r -> r.number) |> List.reduce (*))
        |> List.sum

FirstAnswer.result ()
SecondAnswer.result ()
