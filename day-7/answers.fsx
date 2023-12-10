open System.IO
open System.Text.RegularExpressions

// let filePath = $@"{__SOURCE_DIRECTORY__}\test-input.txt"
let filePath = $@"{__SOURCE_DIRECTORY__}\inputs.txt"
type Card =
    | A = 14
    | K = 13
    | Q = 12
    | J = 11
    | T = 10
    | Nine = 9
    | Eight = 8
    | Seven = 7
    | Six = 6
    | Five = 5
    | Four = 4
    | Three = 3
    | Two = 2

module Card =
    let fromChar =
        function
        | 'A' -> Card.A
        | 'K' -> Card.K
        | 'Q' -> Card.Q
        | 'J' -> Card.J
        | 'T' -> Card.T
        | '9' -> Card.Nine
        | '8' -> Card.Eight
        | '7' -> Card.Seven
        | '6' -> Card.Six
        | '5' -> Card.Five
        | '4' -> Card.Four
        | '3' -> Card.Three
        | '2' -> Card.Two
        | _ -> failwith "Unknown card"

    let value card = int card



type Hand =
    | Hand of Card array

    static member fromLine line =
        line |> Seq.map Card.fromChar |> Array.ofSeq |> Hand

    static member handValue(Hand h) = h |> Array.map Card.value

type HandKind =
    | FiveOfAKind of Hand
    | FourOfKind of Hand
    | FullHouse of Hand
    | ThreeOfAKind of Hand
    | TwoPairs of Hand
    | OnePair of Hand
    | HighCard of Hand

    static member fromHand hand =
        let (Hand cards) = hand

        let g =
            cards
            |> Array.groupBy id
            |> Array.map (fun (_, v) -> Array.length v)
            |> Array.sortDescending

        match g with
        | [| 5 |] -> FiveOfAKind hand
        | [| 4; 1 |] -> FourOfKind hand
        | [| 3; 2 |] -> FullHouse hand
        | [| 3; 1; 1 |] -> ThreeOfAKind hand
        | [| 2; 2; 1 |] -> TwoPairs hand
        | [| 2; 1; 1; 1 |] -> OnePair hand
        | [| 1; 1; 1; 1; 1 |] -> HighCard hand
        | _ -> failwith "WTF ?"

    static member hand handKind =
        match handKind with
        | FiveOfAKind h -> h
        | FourOfKind h -> h
        | FullHouse h -> h
        | ThreeOfAKind h -> h
        | TwoPairs h -> h
        | OnePair h -> h
        | HighCard h -> h

    static member value handKind =
        handKind |> HandKind.hand |> Hand.handValue

type HandAndBid =
    { HandKind: HandKind
      Bid: int }

    static member sort hb1 hb2 =
        let compare hb1 hb2 =
            let firstVals = hb1.HandKind |> HandKind.hand |> Hand.handValue
            let secondVals = hb2.HandKind |> HandKind.hand |> Hand.handValue

            Seq.zip firstVals secondVals
            |> Seq.map (fun (f, s) -> f - s)
            |> Seq.tryFind (fun d -> d <> 0)
            |> Option.defaultValue 0


        match hb1.HandKind, hb2.HandKind with
        | FiveOfAKind _, FiveOfAKind _ -> compare hb1 hb2
        | FiveOfAKind _, _ -> 1
        | _, FiveOfAKind _ -> -1
        | FourOfKind _, FourOfKind _ -> compare hb1 hb2
        | FourOfKind _, _ -> 1
        | _, FourOfKind _ -> -1
        | FullHouse _, FullHouse _ -> compare hb1 hb2
        | FullHouse _, _ -> 1
        | _, FullHouse _ -> -1
        | ThreeOfAKind _, ThreeOfAKind _ -> compare hb1 hb2
        | ThreeOfAKind _, _ -> 1
        | _, ThreeOfAKind _ -> -1
        | TwoPairs _, TwoPairs _ -> compare hb1 hb2
        | TwoPairs _, _ -> 1
        | _, TwoPairs _ -> -1
        | OnePair _, OnePair _ -> compare hb1 hb2
        | OnePair _, _ -> 1
        | _, OnePair _ -> -1
        | HighCard _, HighCard _ -> compare hb1 hb2


let toStuff (line: string) =
    match line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) with
    // | [| hand; bid |] ->
    // { HandKind = hand |> Hand.fromLine |> HandKind.fromHand
    //   Bid = int bid }
    | [| hand; bid |] ->
        { HandKind = hand |> Hand.fromLine |> HandKind.fromHand
          Bid = int bid }
    | _ -> failwith "oups"

// let hands = filePath |> File.ReadAllLines |> Array.map toStuff
// |> Array.sortWith HandAndBid.sort

filePath |> File.ReadAllLines |> Array.map toStuff
//  |> Array.choose (fun h ->
//      match h.HandKind with
//      | ThreeOfAKind  _ -> Some h
//      | _ -> None)
|> Array.sortByDescending (fun h -> h.HandKind |> HandKind.hand)
|> Array.sortWith HandAndBid.sort
|> Array.mapi (fun i h -> (i + 1) * h.Bid)
|> Array.sum

// let firstArray = [| 0..10 |]
// let secondArray = [| 0..10 |]

// Seq.zip firstArray secondArray
// |> Seq.map (fun (f, s) -> s - f)
// |> Seq.tryFind (fun d -> d <> 0)
// |> Option.defaultValue 0
