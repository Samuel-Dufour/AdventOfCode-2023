open System.IO
open System

let filePath = $@"{__SOURCE_DIRECTORY__}\test-input.txt"
type Coords = { cidx: int; ridx: int }

type ArraySpan =
    { topLeft: Coords; bottomRight: Coords }

type NumberArraySpan = { number: int; span: ArraySpan }

module Board =
    let toDigitCoords board =
        let mapper ridx cidx c =
            if Char.IsDigit c then
                Some({ ridx = ridx; cidx = cidx })
            else
                None

        board |> Array2D.mapi mapper

    let toCoordsArray digitCoords =
        [| for ridx in 0 .. Array2D.length1 digitCoords - 1 do
               digitCoords[ridx, *] |> Array.choose id |]
        |> Array.collect id


    let toBoard filePath =
        let linesToArray2D (lines: string[]) =
            let columnsCount =
                lines |> Seq.tryHead |> Option.map Seq.length |> Option.defaultValue 0

            let rowsCount = lines |> Seq.length

            Array2D.init columnsCount rowsCount (fun cidx ridx -> lines[cidx][ridx])

        File.ReadAllLines filePath |> linesToArray2D

    let toIntervalsToCheck board =
        let toDigitSpans board =
            let toArraySpanList coordsArray =
                let folder acc coords =
                    match acc with
                    | [] ->
                        { topLeft = coords
                          bottomRight = coords }
                        :: acc
                    | c :: tail when c.topLeft.ridx = coords.ridx && c.bottomRight.cidx + 1 = coords.cidx ->
                        { c with bottomRight = coords } :: tail
                    | _ ->
                        { topLeft = coords
                          bottomRight = coords }
                        :: acc

                coordsArray |> Array.fold folder List.empty


            let mapper (board: char[,]) span =
                { number =
                    String(board[span.topLeft.ridx, span.topLeft.cidx .. span.bottomRight.cidx])
                    |> int
                  span = span }

            let mapper = mapper board

            board |> toDigitCoords |> toCoordsArray |> toArraySpanList |> List.map mapper

        let mapper nas =
            let maxRidx = Array2D.length1 board - 1
            let maxCidx = Array2D.length2 board - 1

            { nas with
                span =
                    { topLeft =
                        { cidx = max 0 (nas.span.topLeft.cidx - 1)
                          ridx = max 0 (nas.span.topLeft.ridx - 1) }
                      bottomRight =
                        { cidx = min maxCidx (nas.span.bottomRight.cidx + 1)
                          ridx = min maxRidx (nas.span.bottomRight.ridx + 1) } } }

        board |> toDigitSpans |> List.map mapper


module FirstAnswer =
    let getResult () =
        let board = Board.toBoard filePath

        let mapToUsefulRecord nas =
            let boardSpanHasSpecialChar nas =
                let charIsNorDigitNorDot c = Char.IsDigit c |> not && c <> '.'

                let twoDArrayBoolContainsTrue a =
                    [| for ridx in 0 .. Array2D.length1 a - 1 do
                           a[ridx, *] |]
                    |> Array.collect id
                    |> Array.contains true

                board[nas.span.topLeft.ridx .. nas.span.bottomRight.ridx,
                      nas.span.topLeft.cidx .. nas.span.bottomRight.cidx]
                |> Array2D.map charIsNorDigitNorDot
                |> twoDArrayBoolContainsTrue

            if boardSpanHasSpecialChar nas then
                Some nas.number
            else
                None

        let sumOfNumbersToKeep = List.choose id >> List.sum

        board
        |> Board.toIntervalsToCheck
        |> List.map mapToUsefulRecord
        |> sumOfNumbersToKeep

module SecondAnswer =
    type NumberGear = { number: int; gear: Coords }

    let getResult () =
        let board = Board.toBoard filePath

        let mapToGearAndNumberOption r =
            let topleft = r.span.topLeft

            let toBoardSpan =
                board[r.span.topLeft.ridx .. r.span.bottomRight.ridx, r.span.topLeft.cidx .. r.span.bottomRight.cidx]

            let toGearCoordsOption ridx cidx c =
                if c = '*' then
                    Some
                        { ridx = topleft.ridx + ridx
                          cidx = topleft.cidx + cidx }
                else
                    None

            let toGearArray gearCheck =
                [| for ridx in 0 .. Array2D.length1 gearCheck - 1 do
                       gearCheck[ridx, *] |]
                |> Array.collect id
                |> Array.choose id

            let toGearOption gearArray =
                if Array.isEmpty gearArray then
                    None
                else
                    Some
                        { number = r.number
                          gear = gearArray[0] }

            toBoardSpan |> Array2D.mapi toGearCoordsOption |> toGearArray |> toGearOption

        let sumOfGearRatio =
            let listOfGroupHas2Elements (_, l) = List.length l = 2

            let gearRatio (_, l) =
                l |> List.map _.number |> List.reduce (*)

            List.groupBy _.gear
            >> List.filter listOfGroupHas2Elements
            >> List.map gearRatio
            >> List.sum

        board
        |> Board.toIntervalsToCheck
        |> List.map mapToGearAndNumberOption
        |> List.choose id
        |> sumOfGearRatio

FirstAnswer.getResult ()
SecondAnswer.getResult ()
