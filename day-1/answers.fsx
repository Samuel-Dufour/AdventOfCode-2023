open System.IO
open System


let filePath = $@"{__SOURCE_DIRECTORY__}\inputs.txt"

module FirstAnswer =
    let mapLineToDigits line =
        let charToDigit = string >> int
        let digits = line |> Seq.where Char.IsDigit
        let first = digits |> Seq.head |> charToDigit
        let last = digits |> Seq.last |> charToDigit
        first * 10 + last

module SecondAnswer =
    type Digit = { texts: string list; value: int }

    module Digit =
        let seekDigit (s: string) ({ texts = texts; value = value }) =
            texts
            |> List.map (fun t ->
                [ {| index = s.IndexOf t; value = value |}
                  {| index = s.LastIndexOf t
                     value = value |} ]
                |> List.distinct)
            |> List.collect id

    module LineDigits =
        let seekDigit (line: {| index: int; value: int |} list) =
            let sortedDigits = line |> List.sortBy (fun l -> l.index)
            let first = sortedDigits |> List.head
            let last = sortedDigits |> List.last
            first.value * 10 + last.value

    let digits =

        [ { texts = [ "1"; "one" ]; value = 1 }
          { texts = [ "2"; "two" ]; value = 2 }
          { texts = [ "3"; "three" ]; value = 3 }
          { texts = [ "4"; "four" ]; value = 4 }
          { texts = [ "5"; "five" ]; value = 5 }
          { texts = [ "6"; "six" ]; value = 6 }
          { texts = [ "7"; "seven" ]; value = 7 }
          { texts = [ "8"; "eight" ]; value = 8 }
          { texts = [ "9"; "nine" ]; value = 9 } ]

    let mapLineToDigits (line: string) =
        digits
        |> List.map (Digit.seekDigit line)
        |> List.collect id
        |> List.where (fun f -> f.index <> -1)
        |> LineDigits.seekDigit


let firstAnswer =
    filePath
    |> File.ReadAllLines
    |> Array.map FirstAnswer.mapLineToDigits
    |> Array.sum

let secondAnswer =
    filePath
    |> File.ReadAllLines
    |> Array.map SecondAnswer.mapLineToDigits
    |> Array.sum
