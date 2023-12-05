open System.IO
open System

let filePath = $@"{__SOURCE_DIRECTORY__}\inputs.txt"


type ScratchCard =
    { winningNumbers: int array
      cardNumbers: int array }

    static member ofString s =
        let extractNumbers (l: string) =
            match l.Split(":", StringSplitOptions.RemoveEmptyEntries) with
            | [| _; numbers |] -> numbers
            | _ -> failwith "WTF ?"

        let toNumberParts (l: string) =
            let toNumbers (l: string) =
                l.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map (string >> int)

            match l.Split([| '|' |], StringSplitOptions.RemoveEmptyEntries) with
            | [| winningNumbers; myNumbers |] ->
                { winningNumbers = toNumbers winningNumbers
                  cardNumbers = toNumbers myNumbers }
            | _ -> failwith "Nope"

        s |> extractNumbers |> toNumberParts

    static member toValue
        ({ winningNumbers = winNumbers
           cardNumbers = myNumbers })
        =
        match
            myNumbers
            |> Array.where (fun mn -> winNumbers |> Array.contains mn)
            |> Array.length
        with
        | 0 -> 0
        | n -> pown 2 (n - 1)

    static member winingCount
        ({ winningNumbers = winningNumbers
           cardNumbers = cardNumbers })
        =
        cardNumbers
        |> Array.where (fun mn -> winningNumbers |> Array.contains mn)
        |> Array.length

type Card =
    { cardId: int
      numbers: ScratchCard }

    static member ofIndexedLine i l =
        { cardId = i + 1
          numbers = ScratchCard.ofString l }

module FirstAnswer =
    let toValue (line: string) =
        line |> ScratchCard.ofString |> ScratchCard.toValue

    let result () =
        filePath |> File.ReadAllLines |> Array.map toValue |> Array.sum


module SecondAnswer =
    let doMagic lines =
        let cards = lines |> Array.mapi Card.ofIndexedLine

        let initCardsMap =
            cards |> Array.map (fun { cardId = cardId } -> (cardId, 1)) |> Map.ofArray

        let rec fillMap startId inc occurrenceCount (map: Map<int, int>) =
            if occurrenceCount = 0 then
                map
            else
                let cardIdCount = Map.find startId map + inc
                fillMap (startId + 1) inc (occurrenceCount - 1) (Map.add startId cardIdCount map)

        let fillMapFromCards acc cards =
            let cardCount = Map.find cards.cardId acc
            let nextCount = cards.numbers |> ScratchCard.winingCount

            if nextCount = 0 then
                acc
            else
                fillMap (cards.cardId + 1) cardCount nextCount acc


        cards |> Array.fold fillMapFromCards initCardsMap |> Map.values |> Seq.sum

    let result () =
        filePath |> File.ReadAllLines |> doMagic

FirstAnswer.result ()
SecondAnswer.result ()
