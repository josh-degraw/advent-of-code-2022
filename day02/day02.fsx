open System.IO

type Move =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Win
    | Lose
    | Draw

let play (opponent, player) =
    let outcome =
        match player, opponent with
        | Scissors, Paper
        | Paper, Rock
        | Rock, Scissors -> Win
        | Rock, Paper
        | Scissors, Rock
        | Paper, Scissors -> Lose
        | _ -> Draw

    outcome, player

let score (outcome, move) =
    let scoreMove =
        match move with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let scoreOutcome =
        match outcome with
        | Win -> 6
        | Lose -> 0
        | Draw -> 3

    scoreOutcome + scoreMove

let getLines () =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "./input.txt"))

let partOne () =
    getLines ()
    |> Array.map (fun line ->
        let split = line.Split(" ")

        let opponent =
            match split.[0] with
            | "A" -> Rock
            | "B" -> Paper
            | "C" -> Scissors
            | _ -> failwith "Invald input"

        let player =
            match split.[1] with
            | "X" -> Rock
            | "Y" -> Paper
            | "Z" -> Scissors
            | _ -> failwith "Invald input"

        opponent, player)
    |> Array.map (play >> score)
    |> Array.sum


let input =
    getLines ()
    |> Array.map (fun line ->
        let split = line.Split(" ")

        let opponent =
            match split.[0] with
            | "A" -> Rock
            | "B" -> Paper
            | "C" -> Scissors
            | _ -> failwith "Invald input"

        let outcome =
            match split.[1] with
            | "X" -> Lose
            | "Y" -> Draw
            | "Z" -> Win
            | _ -> failwith "Invald input"

        opponent, outcome)

let parseOutcome =
    function
    | Scissors, Lose
    | Paper, Draw
    | Rock, Win -> Paper
    | Rock, Lose
    | Scissors, Draw
    | Paper, Win -> Scissors
    | Paper, Lose
    | Rock, Draw
    | Scissors, Win -> Rock

let partTwo =
    input
    |> Array.map (fun (move, outcome) -> move, parseOutcome (move, outcome))
    |> Array.map (play >> score)
    |> Array.sum
