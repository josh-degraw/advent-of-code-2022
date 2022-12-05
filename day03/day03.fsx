open System.IO

let priorityMap =
    [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] |> List.mapi (fun i c -> c, i + 1) |> Map.ofList

let inputLines =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "./input.txt"))

let sumPriority (chars: char[]) =
    chars |> Array.map (fun a -> Map.find a priorityMap) |> Array.sum

let input (inputLines: string[]) =
    inputLines
    |> Array.map (fun line ->
        let midpoint = line.Length / 2

        let firstHalf = line[ .. midpoint - 1 ].ToCharArray() |> Set
        let secondHalf = line[ midpoint.. ].ToCharArray() |> Set

        Set.intersect firstHalf secondHalf |> Seq.exactlyOne)

let partOne = input inputLines |> sumPriority

let partTwo =
    inputLines
    |> Array.chunkBySize 3
    |> Array.map (Array.map Set >> Set.intersectMany >> Seq.exactlyOne)
    |> sumPriority
