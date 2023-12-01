open CyclicQueue

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

[<EntryPoint>]
let main argv =
    testCreateAndIsEmpty ()
    testEnqueueAndDequeue ()
    testFullQueue ()
    testEmptyQueueDequeue ()
    testQueueLength ()
    testQueueToString ()
    printfn "All tests passed."
    0  // Exit status

