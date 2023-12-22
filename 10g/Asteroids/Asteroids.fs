module Asteroids
open System
open Canvas
open Color

type vec = float * float
type Rotation = Clockwise | CounterClockwise

/// <summary>Multiply a vector with a scalar</summary>
/// <param name="(x,y)">The vector</param>
/// <param name="a">The scalar</param>
/// <returns>(x,y) multiplied by a</returns>
let multiply ((x,y) : vec) (a : float) : vec =
    (x*a, y*a)

/// <summary>Add two vectors.</summary>
/// <param name="(x1,y1)">First vector</param>
/// <param name="(x2,y2)">Second vector</param>
/// <returns>A new vector representing a + b</returns>
let add ((x1,y1) : vec) ((x2,y2) : vec) : vec =
    (x1+y1, x2+y2)

/// <summary>Computes the length of a vector</summary>
/// <param name="(x,y)">The vector</param>
/// <returns>A floating point value indicating the vector's length</returns>
let length ((x,y) : vec) : float =
    Math.Sqrt (x**2.0 + y**2.0)

/// <summary>Given a vector, determine its unit vector</summary>
/// <param name="(x,y)">The vector</param>
/// <returns>A new vector with the same direction but with a length of 1.0</returns>
let unit ((x,y) : vec) : vec =
    let length = length (x,y)
    (x / length, y / length)

/// <summary>Calculate the rotation of a vector from 0 deg around its base</summary>
/// <param name="(x,y)">The vector in question</param>
/// <returns>An angle in radians.</returns>
let rotation ((x,y): vec) : float =
    atan2 y x

[<Interface>]
type IRenderable =
    abstract member Render : unit -> PrimitiveTree

[<AbstractClass>]
type Entity(pos : vec, vel : vec, r : float) =
    let mutable position : vec = pos
    let mutable direction : vec = unit vel
    let mutable speed : float = length vel
    let radius = r
    interface IRenderable with
        member this.Render () = 
            let x,y = position
            let rendation, (rx, ry) = this.RenderInternal()
            (x,y,rendation)
            |||> translate
            |> rotate rx ry (rotation direction)
            
            
    member this.Position
        with get () = position
        and set (value) = position <- value
    member this.Velocity 
        with get () = multiply direction speed
        and set(value) =
            speed <- length value
            direction <- unit value
    member this.Radius = radius

    /// <summary>Using the object's speed, advance it by a given timestep</summary>
    /// <param name="interval">The interval (in seconds) to advance position by</param>
    /// <returns>The new position</returns>
    member this.Advance (interval : float) : vec =
        this.Position <- add this.Position (multiply this.Velocity interval)
        this.Position
    /// <summary>Lets an entity remove itself.</summary>
    /// <returns>false by default, but inheriting classes can override it.</returns>
    abstract member ShouldDie : unit -> bool
    default this.ShouldDie () =
        false
    /// <summary>Convert the current object into a graphics primitive</summary>
    /// <returns>A PrimitiveTree representing the current object.</returns>
    abstract member RenderInternal : unit -> (PrimitiveTree * vec)

[<Sealed>]
type Asteroid(pos : vec, vel : vec, r : float) =
    inherit Entity(pos, vel, r)
    override this.RenderInternal () : (PrimitiveTree * vec) =
        (filledEllipse gray this.Radius this.Radius, (0.0,0.0))

[<Sealed>]
type Bullet(pos : vec, vel : vec) =
    inherit Entity(pos, multiply (unit vel) 20.0, 2.0)
    let _created = DateTime.Now
    member this.Created with get () = _created

    /// <summary>Determine whether or not this bullet has exceeded its lifetime</summary>
    /// <returns>True if the bullet was created more than two seconds ago.</returns>
    override this.ShouldDie () =
        (DateTime.Now - this.Created).Seconds >= 2

    override this.RenderInternal () : (PrimitiveTree * vec) =
        ([(0.0,0.0);
        (10.0,0.0);
        (10.0,1.0);
        (11.0,1.0);
        (11.0,3.0);
        (10.0,3.0);
        (10.0,4.0);
        (0.0,4.0);]
        |> filledPolygon limeGreen
        |> translate -5.0 -2.0, (5.0,2.0))


[<Sealed>]
type Spaceship(pos : vec, vel : vec) =
    inherit Entity(pos, vel, 8)
    member this.Rotate (r : Rotation) = ()
    member this.MakeBullet () = new Bullet((0.0,0.0), (0.0,0.0))
    member this.Accelerate () = ()
    override this.RenderInternal () : (PrimitiveTree * vec) =
        ([(0.0,0.0);
        (38.0,11.0);
        (0.0,22.0)] 
        |> filledPolygon red
        |> translate -8.0 -11.0, (8.0, 11.0))