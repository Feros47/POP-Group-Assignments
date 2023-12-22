#load "Asteroids.fs"
#r "nuget:DIKU.Canvas, 2.0.2"
open Asteroids
open Canvas


[<EntryPoint>]
let main (args : string array) : int =
    let gs = new GameState((512,512), 0.1)
    gs.Run ()