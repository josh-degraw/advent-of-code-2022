open System
open System.IO
open System.Collections.Generic

let inputText =
  File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "./input.txt"))


type Marker (size: int) =
  member val private _window = Queue size

  member this.Ready = this._window.Count = size

  member this.Valid = Set.ofSeq this._window |> Set.count = size

  member this.Push value =
    if this.Ready then
      this._window.TryDequeue () |> ignore

    this._window.Enqueue value
    this.Valid

  override this.ToString () =
    let window = String (this._window |> Seq.toArray)

    sprintf "%s: '%s'" (if this.Valid then "Valid" else "INVALID") window

let findStartMark markerLength inputText =
  let marker = Marker (markerLength)

  inputText |> Seq.findIndex marker.Push |> (+) 1


let partOne = findStartMark 4 inputText
let partTwo = findStartMark 14 inputText
