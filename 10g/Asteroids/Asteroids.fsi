module Asteroids
open Canvas

type vec = float * float
type Rotation = Clockwise | CounterClockwise
exception GameBreakException of bool


[<Interface>]
type IRenderable =
    abstract member Render : unit -> PrimitiveTree

[<AbstractClass>]
type Entity =
    class
        new : vec * vec * float -> Entity
        interface IRenderable
        member Position : vec
        member Velocity : vec
        member Radius : float
        abstract member Direction : vec
        member Advance : float -> (int*int) -> unit
        abstract member ShouldDie : unit -> bool
        abstract member RenderInternal : unit -> (PrimitiveTree * vec)
    end

[<Sealed>]
type Asteroid =
    class
        new : vec * vec * float -> Asteroid
        inherit Entity
    end
[<Sealed>]
type Bullet =
    class
        inherit Entity
        new : vec * vec -> Bullet
        member Created : System.DateTime
    end
[<Sealed>]
type Spaceship =
    class
        inherit Entity
        new : vec * vec * float -> Spaceship
        member Rotate : Rotation -> unit
        member MakeBullet : unit -> Bullet
        member Accelerate : float -> unit
        member Brake : float -> unit
    end

[<Sealed>]
type GameState =
    class
        new : (int*int) * float -> GameState
        member Run : unit -> int
        member Entities : List<Entity>
        member Spaceship : Spaceship

        static member Draw : GameState -> Picture
        static member React : GameState -> Event -> GameState option
    end