module Asteroids
open System
open Canvas
open Color

type vec = float * float
type Rotation = Clockwise | CounterClockwise
exception GameBreakException of bool

/// <summary>Multiply a vector with a scalar</summary>
/// <param name="(x,y)">The vector</param>
/// <param name="a">The scalar</param>
/// <returns>(x,y) multiplied by a</returns>
let multiply ((x,y) : vec) (a : float) : vec =
    (x*a, y*a)
/// <summary>
/// Given two vectors, a and b respectively, subtract b from a element-wise
/// </summary>
/// <param name="(x1, y1)">The 'a' vector</param>
/// <param name="(x2, y2)">The 'b' vector</param>
/// <returns>A vector representing a - b</returns>
let subtract ((x1,y1): vec) ((x2,y2) : vec) : vec =
    (x1 - x2), (y1 - y2)
/// <summary>Add two vectors.</summary>
/// <param name="(x1,y1)">First vector</param>
/// <param name="(x2,y2)">Second vector</param>
/// <returns>A new vector representing a + b</returns>
let add ((x1,y1) : vec) ((x2,y2) : vec) : vec =
    (x1+x2, y1+y2)
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
/// <summary>Determine the shortest of two vectors</summary>
/// <param name="v1">The first vector</param>
/// <param name="v2">The second vector</param>
/// <returns>The shortest of the two vectors.</returns>
let min (v1: vec) (v2 : vec) : vec =
    if (length v1) <= (length v2) then
        v1
    else
        v2
/// <summary>
/// Given a vector and an angle, rotate the vector.
/// </summary>
/// <param name="v">The vector to rotate</param>
/// <param name="rad">The angle in radians (0.0 .. 2.0)</param>
/// <returns>The rotated vector</returns>
let vectorRotate (v : vec) (rad : float) : vec =
    (fst v * Math.Cos(Math.PI * rad) - snd v * Math.Sin(Math.PI * rad)),
    (fst v * Math.Sin(Math.PI * rad) + snd v * Math.Cos(Math.PI * rad))

/// <summary>
/// Compute the dot product of two vectors.
/// </summary>
/// <param name="(x1,x2)">The first vector.</param>
/// <param name="(x2,y2)">The second vector.</param>
/// <returns>A floating point value for the dot product of the two vectors</returns>
let dot ((x1,y1) : vec) ((x2,y2) : vec) : float =
    x1*x2 + y1*y2

[<Interface>]
type IRenderable =
    abstract member Render : unit -> PrimitiveTree

[<AbstractClass>]
type Entity(pos : vec, vel : vec, r : float) =
    let mutable position : vec = pos
    let mutable direction : vec = (0.0,0.0)
    let mutable speed : float = length vel
    let radius = r

    // We initialize direction a little differently since calling unit vel if |vel| = 0 would result in (NaN, NaN)
    // This avoids that problem.
    do
        let (vx,vy) = vel
        if vx = 0 && vy = 0 then
            direction <- (0.0,0.0)
        else
            direction <- unit vel

    interface IRenderable with
        member this.Render () = 
            let x,y = position
            let rendation, (rx, ry) = this.RenderInternal()
            (x,y,rendation)
            |||> translate
            |> rotate rx ry (rotation this.Direction)  
            
    member this.Position
        with get () = position
        and set (value) = position <- value
    member this.Velocity 
        with get () = multiply this.Direction speed
        and set(value) =
            speed <- length value
            this.Direction <- unit value
    member this.Radius = radius
    abstract member Direction : vec with get, set
    default this.Direction
        with get () = direction
        and set(value) = direction <- unit value // Make sure that the value is actually a unit vector.

    /// <summary>Using the object's speed, advance it by a given timestep</summary>
    /// <param name="interval">The interval (in seconds) to advance position by</param>
    /// <returns>The new position</returns>
    member this.Advance (interval : float) ((w, h) : int*int) : unit =
        let mx, my = float w, float h
        let delta = multiply this.Velocity interval
        let xUncapped, yUncapped = add this.Position delta
        this.Position <- ((xUncapped + mx) % mx), ((yUncapped + my) % my)
        //this.Position <- add this.Position (multiply this.Velocity interval)
        //this.Position
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
    // We assume that the 20u_j in the specification refers to muzzle velocity of the bullet and not absolute velocity.
    inherit Entity(pos, add vel (multiply (unit vel) 20.0), 2.0)
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
type Spaceship(pos : vec, orient : vec, acc : float) =
    inherit Entity(pos, (0.0,0.0), 8)
    let _acceleration: float = acc
    let mutable _orientation = unit orient

    override this.Direction 
        with get () = _orientation
        and set (value) = _orientation <- value
    member this.Rotate (r : Rotation) =
        let radIncrement = 0.025
        match r with
            Clockwise -> this.Direction <- (vectorRotate this.Direction radIncrement)
            | _ -> this.Direction <- (vectorRotate this.Direction -radIncrement)
    member this.MakeBullet () =
        let spaceshipTip = add this.Position (multiply (unit this.Direction) 30.0)
        if length this.Velocity <= 0 then
            new Bullet(spaceshipTip, this.Direction)
        else
            new Bullet(spaceshipTip, this.Velocity)
    member this.Accelerate (interval: float) =
        let maxVelocity = multiply (unit this.Direction) 20.0
        let deltaVelocity = multiply this.Direction (interval * _acceleration)
        let newVelocity = add this.Velocity deltaVelocity
        this.Velocity <- min maxVelocity newVelocity
    member this.Brake (interval: float) =
        let minVelocity = (0.0, 0.0)
        let d = this.Direction
        let deltaVelocity = multiply this.Direction (interval * _acceleration)
        let newVelocity = subtract this.Velocity deltaVelocity
        if dot this.Direction newVelocity < 0.0 then
            this.Velocity <- (0.0, 0.0)
        else
            this.Velocity <- newVelocity
        this.Direction <- d // If this.Velocity <- (0.0, 0.0) we have set this.Direction to (0.0, 0.0) and we need to revert this change.
    
    override this.RenderInternal () : (PrimitiveTree * vec) =
        ([(0.0,0.0);
        (38.0,11.0);
        (0.0,22.0)] 
        |> filledPolygon red |> translate -8.0 -11.0, (8.0, 11.0))


[<Sealed>]
type GameState(dims : int * int, timesteps : float) =
    let mutable _entities : List<Entity> = List.empty//[new Asteroid((1.0,128.0), (30.0,30.0), 32.0)]
    let _spaceship : Spaceship = 
        let ss = new Spaceship((256.0,256.0),(5.0,0.0),20.0)
        _entities <- ss :: _entities
        ss

    member this.Entities
        with get() = _entities
        and set(value) = _entities <- value
    member this.Spaceship : Spaceship = _spaceship
    member this.TimeStepSize : float = timesteps
    member this.Width : int = fst dims
    member this.Height : int = snd dims
    member this.Run () : int =
        let mutable error = false
        let delay =
            this.TimeStepSize * 1000.0 // Convert from seconds to ms
            |> int |> Some
        try
            interact "Asteroids" this.Width this.Height delay GameState.Draw GameState.React this
        with 
            | GameBreakException e ->
                if e then
                    printfn "User won the game!"
                else
                    printfn "User lost the game!"
                error <- false
            | exn -> 
                eprintfn "An error occured: %A" exn.Message
                error <- true
        if error then
            1
        else
            0


    member this.CheckCollision (e1 : Entity) (e2 : Entity) =
        let (ex, ey) = e1.Position
        let (ex', ey') = e2.Position
        let dx = ex - ex'
        let dy = ey - ey'
        let distance = sqrt(dx * dx + dy * dy)
        if distance <= (e1.Radius + e2.Radius) then
            Some (e1.GetType(), e2.GetType())
        else
            None
    member this.CheckCollisions () =
        let rec checkCollisions (entities : List<Entity>) =
            match entities with
                | [] -> []
                | e::es ->
                    let collisions = es |> List.collect (fun e' -> 
                        match this.CheckCollision e e' with
                        | Some (type1, type2) when type1 <> typeof<Spaceship> && type2 <> typeof<Spaceship>
                            -> [e; e']
                        | Some (type1, type2) when type1 = typeof<Spaceship> || type2 = typeof<Spaceship>
                            -> raise (GameBreakException(false))
                        | _ -> [])
                    checkCollisions (List.filter (fun e' -> not (List.contains e' collisions)) es)
        this.Entities <- checkCollisions this.Entities
    
    member this.RemoveDeadEntities () : unit = 
        this.Entities <- this.Entities |> List.filter (fun e -> not (e.ShouldDie ()))
    member this.AdvanceEntities () : unit =
        this.Entities |> List.iter (fun e -> e.Advance this.TimeStepSize (this.Width, this.Height))
    static member Draw (state: GameState) : Picture =
        // TODO: Collision detection and resolution
        state.CheckCollisions ()
        state.RemoveDeadEntities ()
        state.AdvanceEntities ()
        let pwa = piecewiseAffine white 2 [(0.0, 256.0); (512.0, 256.0)] |> onto (piecewiseAffine white 2 [(256.0, 0.0); (256.0, 512.0)])
        (pwa, state.Entities)
        ||> List.fold (fun acc e -> acc |> onto ((e :> IRenderable).Render()))
        |> make
        
    static member React (state: GameState) (event: Event) : GameState option =
        match event with
            | TimerTick ->
                Some state
            // Upon user inputs, we deliberately return None since it would otherwise allow users to bypass
            // the given time-interval for updates since holding down a key might send events faster than ticks.
            // This unfortunately has the side effect that movement isn't updated in case a button is held down.
            | RightArrow ->
                state.Spaceship.Rotate(Clockwise)
                None
            | LeftArrow ->
                state.Spaceship.Rotate(CounterClockwise)
                None
            | UpArrow ->
                state.Spaceship.Accelerate state.TimeStepSize
                None
            | DownArrow ->
                state.Spaceship.Brake state.TimeStepSize
                None
            | Key ' ' ->
                state.Entities <- state.Entities @ [state.Spaceship.MakeBullet ()]
                None
            | _ -> None