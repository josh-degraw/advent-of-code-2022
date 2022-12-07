open System
open System.IO
open System.Text.RegularExpressions

let inputText () =
   File.ReadAllLines (Path.Combine (__SOURCE_DIRECTORY__, "./input.txt"))

type TreeNode (name: string, initialContents: NodeContents, parent: TreeNode option) =
   member val private _children =
      match initialContents with
      | Dir contents -> ResizeArray contents
      | _ -> ResizeArray ()

   member val Name = name
   member val Parent = parent

   member this.Contents =
      match initialContents with
      | File size -> File size
      | Dir _ -> Dir (List.ofSeq this._children)

   member this.AddChild next =
      match initialContents with
      | File _ -> invalidOp "Can't add a child to a file node"
      | Dir _ -> this._children.Add (next)

      this

   override this.ToString () =
      String.Join (
         "",
         seq {
            this.Name
            " "

            match this.Contents with
            | Dir _ -> "(dir)"
            | File size -> $"(file, size={size})"
         }
      )

and NodeContents =
   | Dir of children: TreeNode list
   | File of size: int

let rec getTotalSize (node: TreeNode) =
   match node.Contents with
   | File size -> size
   | Dir children -> children |> List.sumBy getTotalSize

let emptyTree = TreeNode ("/", Dir [], None)

let rec getRoot (tree: TreeNode) =
   match tree.Parent with
   | None -> tree
   | Some parent -> getRoot parent

let printTree (tree: TreeNode) =
   let rec printInner indent (current: TreeNode) =
      printfn $"{String (' ', indent)}- %O{current}"

      match current.Contents with
      | File _ -> ()
      | Dir children -> children |> List.iter (printInner (indent + 2))

   printInner 0 (getRoot tree)


let dump (a: TreeNode) =
   printTree a
   a

let getParent (node: TreeNode) =
   node.Parent

let getName (node: TreeNode) =
   node.Name

let (|Output|_|) (input: string) =
   let split = input.Split " "

   if split.Length = 2 then
      Some (split.[0], split.[1])
   else
      None

let (|Command|_|) (input: string) =
   if input.StartsWith "$" then
      Some (input.Substring 2)
   else
      None

let (|Cd|_|) (input: string) =
   match input with
   | Output ("cd", target) -> Some target
   | _ -> None


let (|DirName|_|) (input: string) =
   match input with
   | Output ("dir", name) -> Some name
   | _ -> None

let (|FileSize|_|) (input: string) =
   let reg = Regex """(\d+) (.+)"""
   let regMatch = reg.Match input

   if regMatch.Success then
      Some (regMatch.Groups.[1].Value |> int, regMatch.Groups.[2].Value)
   else
      None

let rec cd next (current: TreeNode) =
   //printfn $"cd {next} from %A{current.Name}"

   match next with
   | "/" -> getRoot current
   | ".." -> current.Parent |> Option.defaultValue current
   | name ->
      match current.Contents with
      | Dir children -> children |> List.find (getName >> ((=) name))
      | File _ -> invalidOp "Can't cd from a file"

let rec addChild next (tree: TreeNode) =
   match tree.Contents with
   | File _ ->
      match tree.Parent with
      | Some parent -> addChild next parent
      | _ -> invalidOp "Cant add a child to a file"
   | Dir _ ->
      let nextNode =
         match next with
         | FileSize (size, name) -> TreeNode (name, File size, Some tree)
         | DirName name -> TreeNode (name, Dir [], Some tree)
         | _ -> invalidOp $"Unknown input: {next}"

      tree.AddChild nextNode


let parseCommand (tree: TreeNode) (input: string) =
   match input with
   | Command "ls" -> tree
   | Command (Cd dir) -> cd dir tree
   | FileSize _
   | DirName _ -> addChild input tree
   | _ -> failwithf "Unknown command %s" input


let root = inputText () |> Array.fold parseCommand emptyTree |> getRoot

let getDirSizes (tree: TreeNode) =
   let rec collect total (next: TreeNode) =
      match next.Contents with
      | File _ -> total @ []
      | Dir contents -> contents |> List.fold collect (total @ [ next.Name, getTotalSize next ])

   collect [] tree


let partOne =
   root
   |> getDirSizes
   |> List.filter (fun (_, size) -> size < 100_000)
   |> List.sumBy snd

let partTwo =
   let all = 70_000_000
   let min = 30_000_000
   let total = getTotalSize root
   let freeSpace = all - total
   let minSize = min - freeSpace

   root
   |> getDirSizes
   |> List.filter (fun (_, size) -> size >= minSize)
   |> List.minBy snd
