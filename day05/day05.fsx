open System
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic

let inputText () =
   File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "./sample-input.txt"))

type Crate = Stack<string>
type Crates = Map<int, Crate>

// Utilities

let len (crate: Crate) =
   crate.Count

let empty () : Crate =
   Crate ()

let printCrates (crates: Crates) =
   let longest = crates.Values |> Seq.maxBy len |> len

   crates.Values
   |> Seq.map (fun crate -> List.init longest (fun i -> Seq.tryItem i (crate |> Seq.rev)))
   |> Seq.toList
   |> List.transpose
   |> List.rev
   |> fun grid ->
         for row in grid do
            for col in row do
               match col with
               | None -> printf "     "
               | Some v -> printf "%-4s " v

            printfn ""

         for i in 1 .. crates.Count do
            printf " %-4d" i

         printfn "\n"

   crates

// Parsing
let parseLine (line: string) =
   let itemRegex = Regex """(\[\w\] ?|   )"""

   itemRegex.Matches line
   |> Seq.cast<Match>
   |> Seq.mapi (fun i match' ->
      match match'.Value.Trim () with
      | "" -> i + 1, None
      | value -> i + 1, Some value
   )
   |> Seq.toList

let parseCrates (input: string) =
   input.Split Environment.NewLine
   |> Array.rev
   |> Array.skip 1
   |> Array.fold
      (fun crates line ->
         parseLine line
         |> List.fold
            (fun crates (id, contents) ->
               crates
               |> Map.change
                  id
                  (fun maybeExisting ->
                     let crate = maybeExisting |> Option.defaultWith empty
                     contents |> Option.iter crate.Push
                     Some crate
                  )
            )
            crates
      )
      Map.empty

let (|Instruction|_|) (input: string) =
   let reg = Regex """move (\d+) from (\d+) to (\d+)"""
   let match' = reg.Match (input)

   if not match'.Success then
      None
   else
      let groups = match'.Groups
      let count, from, to' = groups.[1], groups.[2], groups.[3]

      Some (int count.Value, int from.Value, int to'.Value)

let parseInstructions (input: string) =
   input.Split Environment.NewLine
   |> Seq.map (fun line ->
      match line with
      | Instruction (count, from, to') -> count, from, to'
      | _ -> failwith "Invalid input"
   )

let parseSegments () =
   let split = inputText().Split (Environment.NewLine + Environment.NewLine)

   match split with
   | [| crates; instructions |] -> parseCrates crates, parseInstructions instructions
   | _ -> failwith "Invalid input"

// Solution

let partOne =
   let mutable i = 1
   let crateMap, instructions = parseSegments ()

   printCrates crateMap |> ignore

   for count, from, to' in instructions do

      let fromCrate = crateMap.Item from
      let toCrate = crateMap.Item to'

      printfn $"Step {i}: move {count} from {from} to {to'}"

      for _ in 1..count do
         let item = fromCrate.Pop ()
         toCrate.Push item

         printCrates crateMap |> ignore
         i <- i + 1

   String.Join (
      "",
      seq {
         for crate in crateMap do
            let hasVal, value = crate.Value.TryPeek ()

            if hasVal then
               value.Trim ('[', ']')
      }
   )
