open System
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic

let inputText () =
   File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "./input.txt"))

type Crate = string list //Stack<string>

type Crates = Map<int, Crate>

let push (crate: Crate) a : Crate =
   // crate.Push a
   // crate
   a :: crate
//crate @ [ a ]

let pop (crate: Crate) =
   // let head = crate.Pop ()
   // head, crate
   match crate with
   | head :: tail -> head, tail
   | _ -> failwith "Invalid pop"

let move from to' (crates: Crates) : Crates =
   let item, rest = pop crates[from]

   crates
   |> Map.add from rest
   |> Map.change to' (fun existing -> existing |> Option.map (fun v -> push v item))

let tryPeek (crate: Crate) =
   crate //.TryPeek ()
   |> List.tryHead
   |> function
      | None -> false, null
      | Some v -> true, v

let len (crate: Crate) =
   //crate.Count

   crate.Length

let empty () : Crate =
   []
//Crate ()

let printCrates (crates: Crates) =
   let longest = crates.Values |> Seq.maxBy len |> len

   [
      for crate in crates.Values do
         let reversed = crate |> Seq.rev |> Seq.toArray
         List.init longest (fun i -> Array.tryItem i reversed)
   ]
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

let parseLine (line: string) =
   Regex.Matches (line, """(\[\w\] ?|   )""")
   |> Seq.cast<Match>
   |> Seq.toList
   |> List.mapi (fun i match' ->
      let id = int i + 1

      match match'.Value.Trim () with
      | "" -> id, None
      | value -> id, Some value)

let parseCrates (input: string) =
   input.Split Environment.NewLine
   |> Array.rev
   |> Array.skip 1
   |> Array.rev
   |> Array.fold
         (fun crates line ->
            parseLine line
            |> List.fold
                  (fun crates (id, contents) ->
                     crates
                     |> Map.change id (fun maybeExisting ->
                        let crate = maybeExisting |> Option.defaultWith empty

                        match contents with
                        | Some v -> push crate v
                        | None -> crate
                        |> Some))
                  crates)
         Map.empty
   |> Map.map (fun k v -> v |> List.rev)

let parseInstructions (input: string) =
   let reg = new Regex ("""move (\d+) from (\d+) to (\d+)""")

   [
      for line in input.Split Environment.NewLine do
         let match' = reg.Match line

         if not match'.Success then
            failwith "Invalid input"

         let groups = match'.Groups
         let count, from, to' = groups.[1], groups.[2], groups.[3]

         {|
            Count = int count.Value
            From = int from.Value
            To = int to'.Value
         |}
   ]

let parseSegments () =
   let split = inputText().Split (Environment.NewLine + Environment.NewLine)

   match split with
   | [| crates; instructions |] -> parseCrates crates, parseInstructions instructions
   | _ -> failwith "Invalid input"


let partOne =
   let mutable i = 1
   let crateMap, instructions = parseSegments ()

   printCrates crateMap |> ignore

   let result =
      instructions
      |> List.fold
            (fun crates command ->
               printfn $"Step {i}: move {command.Count} from {command.From} to {command.To}"

               [ 1 .. command.Count ]
               |> List.fold (fun crates _ -> crates |> move command.From command.To) crates
               |> printCrates)
            crateMap

   // for command in instructions do
   //    let fromCrate = crateMap.Item command.From
   //    let toCrate = crateMap.Item command.To

   //    printfn $"Step {i}: move {command.Count} from {command.From} to {command.To}"

   //    for _ in 1 .. command.Count do
   //       let item = pop fromCrate
   //       push toCrate item
   //       // let item = pop fromCrate
   //       // push toCrate item

   //       //printCrates crateMap
   //       i <- i + 1

   String.Join (
      "",
      seq {
         for crate in result do
            let hasVal, value = crate.Value |> tryPeek

            if hasVal then
               value.Trim ('[', ']')
      }
   )
