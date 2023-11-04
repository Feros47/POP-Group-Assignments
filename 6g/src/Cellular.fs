module Cellular

type Pos = int * int
type Size = Pos
type State<'a> = Map<Pos, 'a>
type Rule<'a> = 'a -> 'a list -> 'a
type Update<'a> = State<'a> -> State<'a>


/// <summary>Given a point, determine the values of its Moore neighbourhood</summary>
/// <param name="(x,y)">Coordinates of the point</param>
/// <param name="state">The current values of the grid</param>
/// <param name="(cols,rows)">The size of the grid (starting from 0)</param>
/// <returns>A list with max-length 8 containing the neighbouring values to (x,y)</returns>
let neighbourValues ((cols, rows) : Size) (state: State<'a>) ((x,y) : Pos) : 'a list =
    let predicate ((x,y) : Pos) : bool = (x >= 0 && x < cols) && (y >= 0 && y < rows)
    ([
        (x-1,y-1);
        (x,y-1);
        (x+1,y-1);
        (x-1,y);
        (x+1,y);
        (x-1,y+1);
        (x,y+1);
        (x+1,y+1)
    ] |> List.filter predicate )|> List.map (fun p -> state |> Map.find p)

/// <summary>Given a size and a rule, construct an update function for a cellular automaton</summary>
/// <param name="fst settings">The size of the grid</param>
/// <param name="snd settings">An update rule based on the current value of a cell and its Moore neighbourhood</param>
/// <returns>An update-function capable of determining the next generation's values.</returns>
let cellularAutomaton (settings: Size * Rule<'a>) : Update<'a> =
    let update ((size, rule): Size * Rule<'a>) (state: State<'a>) : State<'a> =
        state |> Map.map (fun (pos : Pos) (value: 'a) -> neighbourValues size state pos |> rule value)
    settings |> update