open System.IO

let inputText =
   File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "./input.txt"))

let findStartMark len (input: string) =
   input
   |> Seq.windowed len
   |> Seq.findIndex (Set >> Set.count >> (=) len)
   |> (+) len

let partOne = findStartMark 4 inputText
let partTwo = findStartMark 14 inputText
