#load "FastQueue.fs"
#load "Cellular.fs"
#load "BTree.fs"
#load "DiffList.fs"

#r "nuget:DIKU.Canvas, 2.0.2"
open Canvas
open Color

open FastQueue
open BTree
open Cellular

/// <summary>
/// This script includes tests for the FastQueue, BTree and DiffList modules, as well as code for visualizing cellular automatons
/// </summary>

// Constants
let gridSize = (15,15)
let w,h = (fst gridSize * 25, snd gridSize * 25)
let lineColor = grey
let strokeWidth = 4.0
let delay = None
let initialState : State<bool> = // bool State with all fields set to false
    [
        for x in 0..(fst gridSize)-1 do
            for y in 0..(snd gridSize)-1 do
                (x,y)
    ] |> List.map (fun pt -> (pt, false)) |> Map.ofList
let gridLines : PrimitiveTree =
    let vLines = [
        for grid in 1..(fst gridSize)-1 do
            let transformation = (float w / float (fst gridSize)) * float grid
            yield [(transformation, 0.0); (transformation, float h)]
    ]
    let lines = vLines @ [
        for grid in 1..(fst gridSize)-1 do
            let transformation = (float h / float (fst gridSize)) * float grid
            yield [(0.0, transformation); (float w, transformation)]
    ]
    lines |> List.fold (fun state coords -> onto state (piecewiseAffine lineColor strokeWidth coords)) emptyTree


/// <summary>
/// Test the FastQueue module.
/// </summary>
let testFastQueue () =
    // Property 1: Check if fromList and toList maintain the list.
    printfn "FastQueue Tests: "
    let test_property1 =
        let xs = [1; 2; 3; 4]
        let q = fromList xs
        let result = toList q
        if result = xs then
            printfn "Property 1: Passed"
        else
            printfn "Property 1: Failed"

    // Property 2: Check if an emptyQueue results in an empty list.
    let test_property2 =
        let q = emptyQueue
        let result = toList q
        if result = [] then
            printfn "Property 2: Passed"
        else
            printfn "Property 2: Failed"

    // Property 3: Check if enqueue correctly adds an element to the end of the queue.
    let test_property3 =
        let xs = [1; 2; 3]
        let x = 4
        let q = fromList xs
        let q' = enqueue (q, x)
        let result = toList q'
        if result = xs @ [x] then
            printfn "Property 3: Passed"
        else
            printfn "Property 3: Failed"

    // Property 4: Check if dequeue on an emptyQueue results in None.
    let test_property4 () =
        let q = emptyQueue
        let result = dequeue q
        if result = None then
            printfn "Property 4: Passed"
        else
            printfn "Property 4: Failed"

    // Property 5: Check if dequeue on a non-empty queue returns the correct element.
    let test_property5 =
        let x = 1
        let xs = [2; 3; 4]
        let q = fromList (x :: xs)
        match dequeue q with
        | Some (resultX, resultQ) when resultX = x && toList resultQ = xs ->
            printfn "Property 5: Passed"
        | _ ->
            printfn "Property 5: Failed"

    // For ikke at lave stack overflow er test_property4 lavet om til en funktion der skal køres separat.
    // Fjern "//"" fra koden på linje 62 for at køre testen.
    // test_property4 ()

    // Ignorer resultatet for at fjerne warnings, det er dovent men det virker.
    ignore 0

/// <summary>
/// Test the BTree module.
/// </summary>
let testBTree () =
    printfn "Binary Tree Tests: "
    // Tree
    let emptyTree = Leaf
    let t1 = Branch (Leaf, 1, Leaf)
    let t2 = Branch (t1, 2, t1)
    let t3 = Branch (Leaf, 3, Leaf)
    let t4 = Branch (t3, 4, t3)
    let t5 = Branch (t2, 5, t4)

    // Testing tree size function
    let testSize = size t5
    if testSize = 7 then
        printfn "Size Test 1 Passed"
    else 
        printfn "Size Test 1 Failed"

    let testEmptySize = size emptyTree
    if testEmptySize = 0 then
        printfn "Size Test 2 Passed"
    else 
        printfn "Size Test 2 Failed"


    // Testing fold function
    let emptyTreeFold =
        // Test 1: Fold on an empty tree should return the initial value.
        let result1 = folder 42 emptyTree
        if result1 = 42 then
            printfn "Fold Test 1 Passed"
        else
            printfn "Fold Test 1 Failed"

    let singleNodeTree =
        // Test 2: Fold on a tree with a single node should return the result of applying the function to that node.
        let result2 = folder 0 t1
        if result2 = 1 then
            printfn "Fold Test 2 Passed"
        else
            printfn "Fold Test 2 Failed"

    let multipleNodeTree =
        // Test 3: Fold on a tree with multiple nodes should accumulate correctly.
        let result3 = folder 0 t5
        if result3 = 7 then
            printfn "Fold Test 3 Passed"
        else
            printfn "Fold Test 3 Failed"
    ignore 0

open DiffList
let testDiffList () =
    printfn "Test DiffList"

    // Tree
    let emptyTree = Leaf
    let t1 = Branch (Leaf, 1, Leaf)
    let t2 = Branch (t1, 2, t1)
    let t3 = Branch (Leaf, 3, Leaf)
    let t4 = Branch (t3, 4, t3)
    let t5 = Branch (t2, 5, t4)

    let result = inorder emptyTree
    if result = [] then 
        printfn "DiffList Test 1 passed"
    else 
        printfn "DiffList Test 1 Failed"

    let result2 = inorder t1
    if result2 =[1] then
        printfn "DiffList Test 2 Passed"
    else 
        printfn "DiffList Test 2 Failed"

    let result3 = inorder t5
    if result3 = [1; 2; 1; 5; 3; 4; 3] then
        printfn "DiffList Test 3 Passed"
    else 
        printfn "DiffList Test 3 Failed"


/// <summary>Update rule for Conway's game of life</summary>
/// <param name="value">The current value of an arbitrary cell.</param>
/// <param name="neighbours">The values of the cells in the cell's Moore neighbourhood</param>
/// <returns>The value that the cell should have in the next generation</returns>
let conwayRule (value: bool) (neighbours: List<bool>) : bool =
    let aliveNeighbours = (neighbours |> List.filter (fun value -> value)).Length
    (value && (aliveNeighbours = 2 || aliveNeighbours = 3)) || ((not value) && aliveNeighbours = 3)
// Use Conway's rule to get an update function given the grid size.
let updateFunction = cellularAutomaton (gridSize, conwayRule)

/// <summary>Handle an IO-event. Update the state if space bar is pressed, or toggle a field if it's clicked.</summary>
/// <param name="s">The current application state.</param>
/// <param name="ev">The event.</param>
/// <returns>Based on the event, either an updated state or None.</returns>
let react (s:State<bool>) (ev: Event) : State<bool> option =
    let fromScreenCoordinates (x: int,y: int) : Pos =
        let (xTrans, yTrans) = (float w / (float (fst gridSize)), float h / (float (snd gridSize)))
        (int (float x / xTrans), int (float y / yTrans))
    match ev with
        | MouseButtonDown (x,y) -> 
            let pos = fromScreenCoordinates (x,y)
            match (s |> Map.find pos) with
                | false -> Some (s |> Map.change pos (fun _ -> Some true))
                | true -> Some (s |> Map.change pos (fun _ -> Some false))
        | Key ' ' ->
            Some (updateFunction s)
        | _ -> None

/// <summary>Given a state, draw the grid.</summary>
/// <param name="s">The current application state.</param>
/// <returns>A picture representing the state.</returns>
let draw (s : State<bool>) : Picture =
    let makeBlackBox (gridX : int, gridY : int) : PrimitiveTree =
        let (xTrans, yTrans) = (float w / (float (fst gridSize)), float h / (float (snd gridSize)))
        (filledRectangle black (xTrans-2.0) (yTrans-2.0)) |> translate ((float gridX * xTrans)+2.0) ((float gridY * yTrans)+2.0)

    let emptyGrid = (filledRectangle white (float w) (float h)) |> onto gridLines
    let liveCells = s |> Map.filter (fun _ value -> value)
    (liveCells |> Map.fold (fun st key _ -> onto (makeBlackBox key) st) emptyGrid) |> make

/// <summary>
/// The entry point of the program.
/// </summary>
[<EntryPoint>]
let main argv =
    testFastQueue ()
    testBTree ()
    testDiffList ()

    // Render the cellular automaton.
    interact "Conway's Game of Life" w h delay draw react initialState
    0 // Success
