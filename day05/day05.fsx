open System
open System.Text
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic

let inputText =
  File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "./sample-input.txt"))

type Crate = { Id: int; Stack: Stack<string> }

type Instruction = { From: int; To: int; Count: int }


let parseLine (line: string) =
  let itemRegex = new Regex("""(\[\w\] ?|   )""")

  [ let mutable i = 1

    for match' in itemRegex.Matches(line) do
      match match'.Value.Trim() with
      | "" -> i, None
      | value -> i, Some value

      i <- i + 1 ]

let parseCrates (input: string) =
  let crates = Dictionary<int, Crate>()

  let lines = input.Split(Environment.NewLine)

  for line in input.Split(Environment.NewLine) |> Array.take (lines.Length - 1) do

    let parsed = parseLine line

    for id, contents in parsed do
      let crate =
        if crates.ContainsKey id then
          crates.Item id
        else
          let newCrate = { Id = id; Stack = Stack() }
          crates.Add(id, newCrate)
          newCrate

      match contents with
      | None -> ()
      | Some c -> crate.Stack.Push(c)

  crates

let parseInstructions (input: string) =
  let reg = new Regex("""move (\d+) from (\d+) to (\d+)""")

  [ for line in input.Split(Environment.NewLine) do
      let match' = reg.Match(line)

      if not match'.Success then
        failwith "Invalid input"

      let groups = match'.Groups
      let count, from, to' = groups.[1], groups.[2], groups.[3]

      { From = int from.Value
        To = int to'.Value
        Count = int count.Value } ]

let crates, instructions =
  let split = inputText.Split(Environment.NewLine + Environment.NewLine)

  match split with
  | [| crates; instructions |] -> parseCrates crates, parseInstructions instructions
  | _ -> failwith "Invalid input"


//let stringify (this: Crate) = String.Join(";", this.Stack)
//crates.Values |> Seq.map stringify |> Seq.toList
