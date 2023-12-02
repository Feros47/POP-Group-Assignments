open CyclicQueue


let testExceptionHandling() =
    assert (dequeue () = None)

let testCreateAndIsEmpty () =
    create 5
    assert (isEmpty ())

let testEnqueueAndDequeue () =
    create 5
    assert (enqueue 1)
    assert (enqueue 2)
    assert ((dequeue ()) = Some 1)
    assert ((dequeue ()) = Some 2)

let testFullQueue () =
    create 2
    assert (enqueue 1)
    assert (enqueue 2)
    assert (not (enqueue 3))  // Queue should now be full

let testEmptyQueueDequeue () =
    create 5
    assert ((dequeue ()) = None)

let testQueueLength () =
    create 5
    assert (enqueue 1)
    assert (enqueue 2)
    assert (length () = 2)

let testQueueToString () =
    create 5
    assert (enqueue 1)
    assert (enqueue 2)
    assert (toString () = "1, 2")

let runTests () =
    let mutable allTestsPassed = true

    let testAndReport testName testFunction =
        try
            testFunction ()
            printfn "%s: Passed" testName
        with
        | _ ->
            printfn "%s: Failed" testName
            allTestsPassed <- false

    testAndReport "testExceptionHandling" testExceptionHandling
    testAndReport "testCreateAndIsEmpty" testCreateAndIsEmpty
    testAndReport "testEnqueueAndDequeue" testEnqueueAndDequeue
    testAndReport "testFullQueue" testFullQueue
    testAndReport "testEmptyQueueDequeue" testEmptyQueueDequeue
    testAndReport "testQueueLength" testQueueLength
    testAndReport "testQueueToString" testQueueToString

    if allTestsPassed then
        printfn "All tests passed."
    else
        printfn "Some tests failed."

[<EntryPoint>]
let main argv =
    runTests ()
    0  // Exit status
