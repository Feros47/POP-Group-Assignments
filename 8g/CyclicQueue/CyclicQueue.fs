module CyclicQueue
open System

type Value = int

// Initialize variables to be None/empty
let mutable first : int option = None
let mutable last : int option = None
let mutable q : Value option[] = [||]

/// <summary>Calculate the next value, cycling over the array</summary>
/// <param name="x">The index</param>
/// <returns>The next value in [0..q.Length - 1]</returns>
let private nextIndex (x: int) : int =
    if x >= q.Length - 1 then
        0
    elif x < 0 then
        0
    else
        x+1

/// <summary>Helper function to check state validity.</summary>
/// <returns>True if state is valid, false otherwise</returns>
let private isValidState () : bool = q.Length > 0

let private isFull () : bool =
    last <> None && first <> None && first = Some (nextIndex (last.Value))

let create (n: int) : unit =
    first <- None
    last <- None
    q <- Array.create (Math.Max (n,0)) None
  
let enqueue (e: Value) : bool =
    // Check for overflows
    if not (isValidState ()) || (isFull ()) then
        false
    else
        if last = None then // Freshly initialized queue.
            first <- Some 0
            last <- Some 0
            q.[last.Value] <- Some e
        else
            last <- Some (nextIndex last.Value)
            q.[last.Value] <- Some e
        true

let dequeue () : Value option =
    if not (isValidState ()) then
        None
    elif first = None then  // Queue is empty
        None
    else
        let tmp = q.[first.Value]
        if first.Value = last.Value then  // Only one element was in the queue
            first <- None
            last <- None
        else
            first <- Some (nextIndex first.Value)
        tmp

let isEmpty () : bool = first = None

let length () : int =
    if not (isValidState()) || (first = None && last = None) then
        0
    else
        (Math.Abs (last.Value - first.Value)) + 1

let toString () : string =
    if isEmpty () then
        ""
    else
        let mutable i : int = first.Value
        let mutable s : string = ""
        while i <> nextIndex last.Value do
            s <- s + (string q.[i].Value + ", ")
            i <- nextIndex i
        s[0..s.Length - 3]
