open System.IO
open System
open System.Numerics

// let filePath = $@"{__SOURCE_DIRECTORY__}\test-input.txt"
let filePath = $@"{__SOURCE_DIRECTORY__}\inputs.txt"

type Seed = Seed of bigint
type Range = 
    {rangeStart:bigint; rangeEnd:bigint}
    static member isInRange ({rangeStart=rangeStart;rangeEnd=rangeEnd}) n =
                                rangeStart <= n && n <= rangeEnd

type Mapping = 
    {destRange:Range; sourceRange:Range}
    static member map ({destRange=destRange; sourceRange=sourceRange}) n =
                                destRange.rangeStart + n - sourceRange.rangeStart
        

type Stuff =
    { seeds: Seed array
      lines: string array
      keys: string array
      maps: Map<string, Mapping array> }
    
let toSeeds (lines: string[]) =
    let toSeeds (seeds: string) =
        seeds.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (string >> BigInteger.Parse >> Seed)

    match lines[0].Split(":") with
    | [| "seeds"; seeds |] ->
        { seeds = toSeeds seeds
          lines = lines[2..]
          maps = Map.empty
          keys = Array.empty }

    | _ -> failwith "Couldn't find seed"

let (|IntRegex|) p s =
    let matchs = System.Text.RegularExpressions.Regex.Match(s, p)
    if matchs.Success|> not then None
    else matchs.Groups |> Seq.skip 1|> Seq.map (_.Value >> BigInteger.Parse) |> Array.ofSeq |> Some

let rec toMap s =
    let toMappings l=
        match l with
        | IntRegex "(\d+) (\d+) (\d+)" (Some [| destRangeStart; sourceRangeStart; rangeLength|]) ->
            {
                destRange={rangeStart=destRangeStart; rangeEnd=destRangeStart + rangeLength}
                sourceRange={rangeStart=sourceRangeStart; rangeEnd=sourceRangeStart + rangeLength}
            }
        | _ -> failwith "oups"

    
    if Array.isEmpty s.lines then
        s
    else
        let key =
            match s.lines[0].Split(' ', StringSplitOptions.RemoveEmptyEntries) with
            | [| key; "map:" |] -> key
            | _ -> failwith "It's not a map"

        let definitions =
            s.lines[1..]
            |> Array.takeWhile (fun s -> System.String.IsNullOrEmpty s |> not)
        let map =
            definitions
            |> Array.map toMappings
    
        toMap
         { s with
                lines = s.lines[(Array.length definitions + 2) ..]
                keys = [|key|] |> Array.append s.keys
                maps = s.maps.Add(key, map) }


let applyMappings r =
   
    r.seeds
    |> Array.map (fun (Seed s) -> 
        Array.mapFold (fun acc k ->
            // let (_, h) = List.head acc
            match Map.find k r.maps |> Array.tryFind (fun m -> Range.isInRange m.sourceRange acc) with
            | None -> (k, acc), acc
            | Some range -> 
                let v = Mapping.map range acc
                (k, v), v)
            s
            r.keys) 

File.ReadAllLines filePath |> toSeeds |> toMap
|> applyMappings 
|> Array.map (fun (_, r) -> r)
|> Array.min

    