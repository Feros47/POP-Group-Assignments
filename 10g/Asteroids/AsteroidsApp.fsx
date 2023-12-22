#load "Asteroids.fs"
#r "nuget:DIKU.Canvas, 2.0.2"
open Asteroids
open Canvas


let pwa =
    piecewiseAffine Color.white 1 [(0.0, 256.0); (512.0, 256.0)]
    |> onto (piecewiseAffine Color.white 1 [(256.0, 0.0); (256.0, 512.0)];)

let ss = new Bullet((256.0,256.0), (2.0,-1.0))
let renderable = (ss :> IRenderable)
let tree = 
    renderable.Render ()
    |> onto pwa
let draw = tree |> make
render "Test" 512 512 draw