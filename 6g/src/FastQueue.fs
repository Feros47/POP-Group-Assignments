module FastQueue

/// <summary>
/// Represents a functional queue, implemented as a pair of two lists.
/// </summary>
type Queue<'a> = 'a list * 'a list

/// <summary>
/// Creates an empty queue.
/// </summary>
let emptyQueue : Queue<'a> = ([], [])

/// <summary>
/// Enqueues an element at the end of the queue.
/// </summary>
/// <param name="queue">The input queue.</param>
/// <param name="x">The element to enqueue.</param>
/// <returns>A new queue with the element enqueued at the end.</returns>
let enqueue (((frontq, endq), x) : Queue<'a> * 'a) : Queue<'a> =
    (frontq, x :: endq)

/// <summary>
/// Dequeues an element from the front of the queue.
/// </summary>
/// <param name="queue">The input queue.</param>
/// <returns>An option containing the dequeued element and the updated queue, or None if the queue is empty.</returns>
let rec dequeue ((frontq, endq) : Queue<'a>) : ('a * Queue<'a>) option =
    match frontq with
    | [] -> dequeue (List.rev endq, [])
    | x :: xs -> Some (x, (xs, endq))

/// <summary>
/// Creates a queue from a list of elements.
/// </summary>
/// <param name="l">The list of elements to create the queue from.</param>
/// <returns>A queue containing the elements from the input list.</returns>
let fromList (l : 'a list) : Queue<'a> =
    (l, [])

/// <summary>
/// Converts a queue to a list.
/// </summary>
/// <param name="queue">The input queue.</param>
/// <returns>A list containing the elements in the queue, in the order they were enqueued.</returns>
let toList ((q1, q2) : Queue<'a>) : 'a list =
    q1 @ List.rev q2
