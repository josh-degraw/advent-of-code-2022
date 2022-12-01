open System
open System.IO

let readInput (fileName: string) =
    seq {
        use reader = new StreamReader(fileName)

        let mutable i = 1
        let mutable currentTotal = 0
        while reader.EndOfStream |> not do
            let next = reader.ReadLine()
            //printfn $"{currentTotal} - {next}"
            if String.IsNullOrWhiteSpace next then 
                yield currentTotal, i
                currentTotal <- 0
                i <- i + 1
            else
                currentTotal <- currentTotal + int(next)
        yield currentTotal , i
    }


let partOne()=
    let input = readInput "./day01/input.txt"

    input 
    |> Seq.map fst
    |> Seq.max
    |> printfn "Max: %i"


//partOne()

let partTwo()=
    let input = readInput "./day01/input.txt"

    input 
    |> Seq.sortByDescending fst
    |> Seq.take 3
    |> Seq.toList
    |> fun top -> (top), (top|> List.map fst |> List.sum)
    |> printfn "Top 3: %A"

partTwo()