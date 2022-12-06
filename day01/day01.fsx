open System
open System.IO

let split (delim: string) (str: string) =
  str.Split delim

let input =
  File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "./input.txt"))
  |> split (Environment.NewLine + Environment.NewLine)
  |> Array.map ((split Environment.NewLine) >> Array.sumBy int)

let partOne () =
  input |> Array.max |> printfn "Max: %i"

let partTwo () =
  input
  |> Array.sortDescending
  |> Array.take 3
  |> Array.sum
  |> printfn "Top 3 Sum: %A"

partOne ()
partTwo () // 204639
