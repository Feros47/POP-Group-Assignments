module Asteroids
open Canvas

type vec = float * float
type Rotation


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
        member Advance : float -> vec
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
        new : vec * vec -> Spaceship
        member Rotate : Rotation -> unit
        member MakeBullet : unit -> Bullet
        member Accelerate : unit -> unit
    end