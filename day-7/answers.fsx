open System.IO

// let filePath = $@"{__SOURCE_DIRECTORY__}\test-input.txt"
let filePath = $@"{__SOURCE_DIRECTORY__}\inputs.txt"

type Card =
    | A = 14
    | K = 13
    | Q = 12
    | T = 10
    | Nine = 9
    | Eight = 8
    | Seven = 7
    | Six = 6
    | Five = 5
    | Four = 4
    | Three = 3
    | Two = 2
    | J = 1

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
        | a -> failwith $"WTF ? %A{a}"

    static member fromHand2(Hand cards) =
        let allCards = cards |> Array.countBy id //|> Map.ofArray
        let map = allCards |> Map.ofArray

        match Map.tryFind Card.J map with
        | Some n when n = 5 -> 
            Map.add Card.A 5 Map.empty
            |> Map.map (fun k v -> Array.create v k)
            |> Map.values |> Seq.collect id |> Seq.toArray
        | Some n ->
            let card = map |> Map.keys |> Seq.max
            let currentN = map |> Map.find card
            let m = map |> Map.add card (currentN + n) |> Map.add Card.J 0
            let rezr = Map.map (fun k v -> Array.create v k) m
            rezr |> Map.values |> Seq.collect id |> Seq.toArray

        | None -> cards
        |> Hand
        |> HandKind.fromHand

    static member fromHand3(Hand cards) =
        let jCount = cards|> Array.filter (fun c -> c = Card.J)|> Array.length
        if jCount = 0 then cards
        else 
            let cards = cards|> Array.filter (fun c -> c <> Card.J) |> Array.countBy id |> Array.sortByDescending (fun (c, n) -> n, int c)
            match cards|> Array.tryHead with
            | Some (c, n) -> 
                let map = cards |> Map.ofArray
                let rezr = map|> Map.add c (n + jCount)|> Map.map (fun c n -> Array.create n c)
                rezr |> Map.values|> Seq.collect id|> Seq.toArray
            | None -> Array.create 5 Card.A
        |> Hand
        |> HandKind.fromHand



    static member hand handKind =
        match handKind with
        | FiveOfAKind h -> h
        | FourOfKind h -> h
        | FullHouse h -> h
        | ThreeOfAKind h -> h
        | TwoPairs h -> h
        | OnePair h -> h
        | HighCard h -> h


type HandAndBid =
    { HandKind: HandKind
      RealHand: Hand
      Bid: int64 }

    static member sort hb1 hb2 =
        let compare hb1 hb2 =
            let firstVals = hb1.RealHand |> Hand.handValue
            let secondVals = hb2.RealHand |> Hand.handValue

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



let toStuff' (line: string) =
    match line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) with
    | [| hand; bid |] ->
        { HandKind = hand |> Hand.fromLine |> HandKind.fromHand3
          RealHand = hand |> Hand.fromLine
          Bid = int64 bid }

    | _ -> failwith "oups"



filePath |> File.ReadAllLines 
|> Array.map toStuff' //|> Array.map yolo
|> Array.sortWith HandAndBid.sort
|> Array.mapi (fun i h -> (int64 i + 1L) * h.Bid)
|> Array.sum
