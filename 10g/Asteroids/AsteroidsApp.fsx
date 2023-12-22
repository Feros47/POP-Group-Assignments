#load "Asteroids.fs"
#r "nuget:DIKU.Canvas, 2.0.2"
open Asteroids
open Canvas

let bullet = (new Spaceship((256.0,256.0), (1.0,1.0)) :> IRenderable)
let tree = bullet.Render () |> onto ((filledEllipse Color.green 8.0 8.0) |> translate 256.0 256.0)
let draw = tree |> make
render "Test" 512 512 draw