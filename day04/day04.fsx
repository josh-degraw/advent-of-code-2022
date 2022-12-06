open System.IO

let inputLines =
  File.ReadAllLines (Path.Combine (__SOURCE_DIRECTORY__, "./input.txt"))

let isBetween (start: int) (finish: int) (v: int) =
  v >= start && v <= finish

type Range =
  | Range of int * int

  member this.Contains (Range (otherStart, otherEnd)) =
    let (Range (start, finish)) = this
    start <= otherStart && finish >= otherEnd

  member this.Overlaps (Range (otherStart, otherEnd)) =
    let (Range (start, finish)) = this

    isBetween start finish otherStart || isBetween start finish otherEnd

let rangesFullyOverLap (a: Range, b: Range) =
  a.Contains b || b.Contains a

let rangesPartiallyOverlap (a: Range, b: Range) =
  a.Overlaps b || b.Overlaps a

let pairs =
  let parsePair (str: string) =
    match str.Split ("-") with
    | [| start; finish |] -> Range (int start, int finish)
    | _ -> failwith "Invalid range"

  inputLines
  |> Array.map (fun line ->
    match line.Split (",") with
    | [| a; b |] -> parsePair a, parsePair b
    | _ -> failwith "Invalid input")


let partOne = pairs |> Array.filter rangesFullyOverLap |> Array.length

let partTwo = pairs |> Array.filter rangesPartiallyOverlap |> Array.length
