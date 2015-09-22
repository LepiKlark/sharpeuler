// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
type Direction = Left | Right | Up | Down

[<EntryPoint>]
let main argv = 
    let path = @"C:\users\atomic\desktop\euler81.txt"
    let matrix = System.IO.File.ReadAllLines path |> Array.map (fun str -> str.Split(',') |> Array.map (System.Int32.Parse))
    let min x y z k = [x;y;z;k] |> List.min
    let x1, y1 = 79, 79
    let FAKE_LARGE = 100 * 1000 * 80

    let rec traverse pos d (mins : option<int> [,]) beenThere =
        let x, y = pos
        let prevPos = match d with | Right -> 0  | Left -> 1 | Up -> 2 | Down -> 3
        if Set.contains pos beenThere then FAKE_LARGE
        elif beenThere.Count > 200 then FAKE_LARGE
        elif y = y1 && x = x1 then 
            printfn "FOUND!!!"
            matrix.[x].[y]
        elif mins.[x, y].IsSome then
            mins.[x, y].Value
        else                        
            let right = if y < y1 && not (d = Left) then traverse (x, y + 1) Right mins (beenThere.Add pos) else FAKE_LARGE
            let down = if x < x1 && not (d = Up) then traverse (x + 1, y) Down mins (beenThere.Add pos)     else FAKE_LARGE
            let up = if x > 0 && not (d = Down) then traverse (x - 1, y) Up mins (beenThere.Add pos)        else FAKE_LARGE
            let left = if y > 0 && not (d = Right) then traverse (x, y - 1) Left mins (beenThere.Add pos)   else FAKE_LARGE
            let v = (min down right up left) + matrix.[x].[y]
            mins.[x, y] <- Some v
            v

    let mins : option<int> [,] = Array2D.init 80 80 (fun i j -> None)
    let sol = traverse (0, 0) Right mins (Set.empty)


    printfn "%A" sol
    0
