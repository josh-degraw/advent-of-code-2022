open System
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic

let inputText () =
  File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "./input.txt"))

type Crate = Stack<string>

type Instruction =
  {
    From: int
    To: int
    Count: int
  }

let printCrates (crates: Map<int, Crate>) =
  for crate in crates do
    printfn "%d:  %s" crate.Key (String.Join (" ", crate.Value))

  printfn ""

let parseLine (line: string) =
  let itemRegex = new Regex ("""(\[\w\] ?|   )""")

  [
    let mutable i = 1

    for match' in itemRegex.Matches (line) do
      match match'.Value.Trim () with
      | "" -> i, None
      | value -> i, Some value

      i <- i + 1
  ]

let parseCrates (input: string) =
  input.Split (Environment.NewLine)
  |> Array.rev
  |> Array.skip 1
  |> Array.fold
       (fun crates line ->
         parseLine line
         |> List.fold
              (fun crates (id, contents) ->
                crates
                |> Map.change id (fun maybeExisting ->
                  let crate = maybeExisting |> Option.defaultWith Crate
                  contents |> Option.iter crate.Push
                  Some crate))
              crates)
       Map.empty


let parseInstructions (input: string) =
  let reg = new Regex ("""move (\d+) from (\d+) to (\d+)""")

  [
    for line in input.Split (Environment.NewLine) do
      let match' = reg.Match (line)

      if not match'.Success then
        failwith "Invalid input"

      let groups = match'.Groups
      let count, from, to' = groups.[1], groups.[2], groups.[3]

      {
        From = int from.Value
        To = int to'.Value
        Count = int count.Value
      }
  ]

let parseSegments () =
  let split = inputText().Split (Environment.NewLine + Environment.NewLine)

  match split with
  | [| crates; instructions |] -> parseCrates crates, parseInstructions instructions
  | _ -> failwith "Invalid input"


let partOne =
  let mutable i = 1
  let crateMap, instructions = parseSegments ()

  printCrates crateMap

  for command in instructions do

    let fromCrate = crateMap.Item command.From
    let toCrate = crateMap.Item command.To

    printfn $"Step {i}: move {command.Count} from {command.From} to {command.To}"

    for _ in 1 .. command.Count do
      let item = fromCrate.Pop ()
      toCrate.Push item

      printCrates crateMap
      i <- i + 1

  seq {
    for crate in crateMap do
      let hasVal, value = crate.Value.TryPeek ()

      if hasVal then
        value.Trim ('[', ']')
  }
  |> fun str -> String.Join ("", str)
