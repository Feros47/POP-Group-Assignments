module CyclicQueue

type Value = int

let mutable q : Value option [] = [||]
let mutable first : int option = None
let mutable last : int option = None

let create n = 
    q <- Array.create n None
    first <- None
    last <- None

let private isFull () =
    match first, last with
    | Some f, Some l -> (l + 1) % Array.length q = f
    | _ -> false

let enqueue e =
    if isFull () then false
    else
        match last with
        | None ->
            first <- Some 0
            last <- Some 0
            q.[0] <- Some e
            true
        | Some l ->
            let newLast = (l + 1) % Array.length q
            q.[newLast] <- Some e
            last <- Some newLast
            true

let dequeue () =
    match first, last with
    | Some f, Some l when f = l ->
        let result = q.[f]
        first <- None
        last <- None
        result
    | Some f, Some _ ->
        let result = q.[f]
        first <- Some ((f + 1) % Array.length q)
        result
    | _ -> None

let isEmpty () = 
    first = None

let length () = 
    match first, last with
    | Some f, Some l when f <= l -> l - f + 1
    | Some f, Some l -> Array.length q - f + l + 1
    | _ -> 0

let toString () =
    q
    |> Array.choose id
    |> Array.map string
    |> String.concat ", "
