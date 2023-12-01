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
let nextIndex (x: int) : int =
    if x >= q.Length - 1 then
        0
    elif x < 0 then
        0
    else
        x+1

/// <summary>Helper function to check state validity.</summary>
/// <returns>True if state is valid, false otherwise</returns>
let isValidState () : bool =
    last <> None && first <> None && q.Length > 0

let create (n: int) : unit =
    if n <= 0 then
        failwith "invalid argument: cannot create queue with nonpositive size"
    else
        // Reset first and last in case the queue already exists.
        first <- Some 0
        last <- Some 0
        q <- Array.create n None

  
let enqueue (e: Value) : bool =
    // Check for overflows
    if not (isValidState ()) || ((nextIndex last.Value) = first.Value) then
        false
    else
        last <- Some (nextIndex last.Value)
        q.[last.Value] <- Some e
        true


let dequeue () : Value option =
    if not (isValidState ()) then
        failwith "invalid operation: Cannot dequeue from a non-initialized queue."
    elif first.Value = last.Value then
        None
    else
        let tmp = q.[first.Value]
        first <- Some (nextIndex first.Value)
        tmp

let isEmpty () : bool =
    not (isValidState ()) || first.Value = last.Value


let length () : int =
    if not (isValidState()) then
        0
    else
        Math.Abs (last.Value - first.Value)

let toString () : string =
    if not (isValidState ()) then
        ""
    else
        let mutable i : int = first.Value
        let mutable s : string = ""
        while i <> last.Value do
            s <- s + (string q.[i])
            i <- nextIndex i
        s
