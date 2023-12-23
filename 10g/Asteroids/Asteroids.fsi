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
        abstract member HandleCollision : unit -> List<Entity>
        abstract member RenderInternal : unit -> (PrimitiveTree * vec)
        static member CheckCollision : Entity -> Entity -> bool
    end

[<Sealed>]
type Asteroid =
    class
        new : vec * vec * float -> Asteroid
        inherit Entity
        static member MaxSpeed : float
        member private findChildPosition : unit -> vec
        member private randomVelocity : unit -> vec
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
        new : (int*int) * int * float * bool -> GameState
        member Entities : List<Entity> with get, set
        member Spaceship : Spaceship
        member TimeStepSize : float
        member Width : int
        member Height : int

        member Run : unit -> int
        member update : unit -> unit
        member CheckCollisions : unit -> unit
        member RemoveDeadEntities : unit -> unit
        member AdvanceEntities : unit -> unit
        static member Draw : GameState -> Picture
        static member React : GameState -> Event -> GameState option
    end