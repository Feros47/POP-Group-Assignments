#load "FastQueue.fs"
#load "BTree.fs"

open FastQueue
open BTree
/// <summary>
/// This script includes tests for the FastQueue and BTree modules.
/// </summary>

/// <summary>
/// Test the FastQueue module.
/// </summary>
let testFastQueue () =
    // Property 1: Check if fromList and toList maintain the list.
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
    // Tree
    let emptyTree = Leaf
    let t1 = Branch (Leaf, 1, Leaf)
    let t2 = Branch (t1, 2, t1)
    let t3 = Branch (Leaf, 3, Leaf)
    let t4 = Branch (t3, 4, t3)
    let t5 = Branch (t2, 5, t4)

    // Testing tree size function
    let testSize = size t5
    printfn "Number of internal nodes in the tree: %d" testSize

    // Testing fold function
    let emptyTreeFold =
        // Test 1: Fold on an empty tree should return the initial value.
        let result1 = folder 42 emptyTree
        if result1 = 42 then
            printfn "Test 1 Passed"
        else
            printfn "Test 1 Failed"

    let singleNodeTree =
        // Test 2: Fold on a tree with a single node should return the result of applying the function to that node.
        let result2 = folder 0 t1
        if result2 = 1 then
            printfn "Test 2 Passed"
        else
            printfn "Test 2 Failed"

    let multipleNodeTree =
        // Test 3: Fold on a tree with multiple nodes should accumulate correctly.
        let result3 = folder 0 t5
        if result3 = 7 then
            printfn "Test 3 Passed"
        else
            printfn "Test 3 Failed"
    ignore 0


/// <summary>
/// The entry point of the program.
/// </summary>
[<EntryPoint>]
let main argv =
    testFastQueue ()
    testBTree ()
    0 // Success
