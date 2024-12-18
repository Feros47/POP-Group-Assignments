module Asteroids
open System
open Canvas
open Color

type vec = float * float
type Rotation = Clockwise | CounterClockwise
exception GameBreakException of bool

// Global random number generator.
// These are seeded with a default value, so by making it global we in effect add randomness as a result of how the user chooses to play.
let rng = new Random()

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

/// <summary>Generate a random number that can both be positive and negative</summary>
/// <param name="rng">The random number generator to use</param>
/// <returns>An integer between -Int.MaxValue and Int.MaxValue</returns>
let randEntireRange () : int =
    let sign = (rng.Next() % 2) = 0
    if sign then
        rng.Next()
    else
        -rng.Next()

/// <summary>Generate a random vector within a certain length range</summary>
/// <param name="minLength">Minimum inclusive length</param>
/// <param name="maxLength">Maximum exclusive length</param>
/// <returns>A vector with random orientation and random length withing [minLength; maxLength[</returns>
let randVectorInLengthRange (minLength: float) (maxLength : float) =
    let randomLength = minLength + (maxLength - minLength) * rng.NextDouble()
    let randomAngle = 2.0 * Math.PI * rng.NextDouble ()
    (randomLength * (Math.Cos randomAngle), randomLength * (Math.Sin randomAngle))

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
        /// <summary>Calls RenderInternal to get a representation of the current object, translates and rotates this</summary>
        /// <returns>A PrimitiveTree object representing the current object and its position</returns>
        member this.Render () : PrimitiveTree = 
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

    /// <summary>Lets an entity remove itself.</summary>
    /// <returns>false by default, but inheriting classes can override it.</returns>
    abstract member ShouldDie : unit -> bool
    default this.ShouldDie () =
        false

    /// <summary>Handle a collision.</summary>
    abstract member HandleCollision : unit -> List<Entity>
    /// <summary>Convert the current object into a graphics primitive</summary>
    /// <returns>A PrimitiveTree representing the current object, and a rotation axis to use.</returns>
    abstract member RenderInternal : unit -> (PrimitiveTree * vec)

    /// <summary>Check if two objects collide</summary>
    /// <param name="e1">The first object</param>
    /// <param name="e2">The second object</param>
    /// <returns>True if they collide, false otherwise</returns>
    static member CheckCollision (e1 : Entity) (e2 : Entity) : bool =
        let (ex, ey) = e1.Position
        let (ex', ey') = e2.Position
        let dx = ex - ex'
        let dy = ey - ey'
        let distance = sqrt(dx * dx + dy * dy)
        if distance <= (e1.Radius + e2.Radius) then
            true
        else
            false

[<Sealed>]
type Asteroid(pos : vec, vel : vec, r : float) =
    inherit Entity(pos, min vel (multiply (unit vel) Asteroid.MaxSpeed), r)    
    static member MaxSpeed = 10.0;
    override this.HandleCollision () : List<Entity> =
        // If our radius is <= 8, we don't need to check anything and can just return
        // since this asteroid should just be removed.
        if this.Radius <= 8 then
            []
        else
            let child1RelativePosition = this.findChildPosition ()
            let child2RelativePosition = multiply child1RelativePosition -1.0
            [new Asteroid(
                add this.Position child1RelativePosition,
                this.randomVelocity (),
                this.Radius / 2.0);
            new Asteroid(
                add this.Position child2RelativePosition,
                this.randomVelocity (),
                this.Radius / 2.0)]
    override this.RenderInternal () : (PrimitiveTree * vec) =
        (filledEllipse gray this.Radius this.Radius, (0.0,0.0))

    /// <summary>Generate a vector with random direction as a candidate for a child asteroid</summary>
    /// <param name="entities">The entities on the window.</param>
    /// <returns>A vector to the new child's position</returns>
    member private this.findChildPosition () : vec =
        let randomUnitVector () : vec =
            unit (float (randEntireRange ()), float (randEntireRange ()))
        (multiply (randomUnitVector ()) this.Radius)
    
    /// <summary>Generate a velocity with speed between |this.Velocity| and MaxSpeed, with a random orientation</summary>
    /// <returns>The new velocity vector used for child velocity calculation</returns>
    member private this.randomVelocity () : vec =
        let clamp (minVec : vec) (value : vec) (maxVec : vec) : vec=
            min (max minVec value) maxVec
        
        // Create a vector with a random speed between the current speed and the maximum allowed speed
        clamp 
            this.Velocity 
            (randVectorInLengthRange (length this.Velocity) Asteroid.MaxSpeed)
            (multiply this.Direction Asteroid.MaxSpeed)

[<Sealed>]
type Bullet(pos : vec, vel : vec) =
    // We assume that the 20u_j in the specification refers to muzzle velocity of the bullet and not absolute velocity.
    inherit Entity(pos, add vel (multiply (unit vel) 20.0), 2.0)
    let _created = DateTime.Now
    member this.Created with get () = _created
    override this.ShouldDie () =
        (DateTime.Now - this.Created).Seconds >= 2
    override this.HandleCollision () : List<Entity> =
        []
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
    /// <summary>Rotates the spaceship's nose by 0.025 radians</summary>
    /// <param name="r">Whether to rotate clockwise or counter clockwise</param>
    member this.Rotate (r : Rotation) =
        let radIncrement = 0.025
        match r with
            Clockwise -> this.Direction <- (vectorRotate this.Direction radIncrement)
            | _ -> this.Direction <- (vectorRotate this.Direction -radIncrement)
    
    /// <summary>
    /// Use the current speed and orientation to create a Bullet object originating from the spaceships nose.
    /// </summary>
    member this.MakeBullet () =
        let spaceshipTip = add this.Position (multiply (unit this.Direction) 30.0)
        if length this.Velocity <= 0 then
            new Bullet(spaceshipTip, this.Direction)
        else
            new Bullet(spaceshipTip, this.Velocity)

    /// <summary>Accelerate the spaceship</summary>
    /// <param name="interval">The interval in which we will accelerate, in seconds</param>
    member this.Accelerate (interval: float) =
        let maxVelocity = multiply (unit this.Direction) 20.0
        let deltaVelocity = multiply this.Direction (interval * _acceleration)
        let newVelocity = add this.Velocity deltaVelocity
        this.Velocity <- min maxVelocity newVelocity
    
    /// <summary>Decelerate the spaceship</summary>
    /// <param name="interval">The interval in which we will brake, in seconds</param>
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
    override this.HandleCollision () : List<Entity> =
        raise (GameBreakException(false))    
    override this.RenderInternal () : (PrimitiveTree * vec) =
        ([(0.0,0.0);
        (38.0,11.0);
        (0.0,22.0)] 
        |> filledPolygon red |> translate -8.0 -11.0, (8.0, 11.0))


[<Sealed>]
type GameState(dims : int * int, numInitialAsteroids: int, timesteps : float, empty : bool) =
    let mutable _entities : List<Entity> = []
    let _spaceship : Spaceship = 
        let largestPossibleVector = (float (fst dims), float (snd dims))
        let pos = randVectorInLengthRange 0.0 (length largestPossibleVector)
        let orientation = unit (randVectorInLengthRange 0.0 (length largestPossibleVector))
        let ss = new Spaceship(pos,orientation,20.0)
        if not empty then
            _entities <- ss :: _entities
        else
            ()
        ss

    /// <summary>Create an asteroid with a random position and velocity, not colliding with anything else</summary>
    /// <returns>A randomly placed asteroid object</returns>
    let createRandomAsteroid () : Asteroid =
        let largestPossibleVector = (float (fst dims), float (snd dims))
        let mutable currentPosition = randVectorInLengthRange 0.0 (length largestPossibleVector)
        let mutable asteroid = new Asteroid (currentPosition, (0.0, 0.0), 32.0)

        while (_entities |> List.tryFind (fun e -> (Entity.CheckCollision e asteroid))).IsSome do
            currentPosition <- randVectorInLengthRange 0.0 (length largestPossibleVector)
            asteroid <- new Asteroid (currentPosition, (0.0, 0.0), 32.0)
        // We now have a valid position -> create a random velocity and return
        let velocity = randVectorInLengthRange 1.0 Asteroid.MaxSpeed
        new Asteroid(currentPosition, velocity, 32.0)

    do
        if not empty then
            for _ in [1..numInitialAsteroids] do
                _entities <- _entities @ [createRandomAsteroid ()]

    member this.Entities
        with get() = _entities
        and set(value) = _entities <- value
    member this.Spaceship : Spaceship = _spaceship
    member this.TimeStepSize : float = timesteps
    member this.Width : int = fst dims
    member this.Height : int = snd dims
    /// <summary>Run the game!</summary>
    /// <returns>An integer indicating whether or not an error occured (0 for success)</returns>
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

    /// <summary>Update the current game state</summary>
    member this.update () : unit =
        if (this.Entities |> List.filter (fun e -> (e.GetType() = typeof<Asteroid>))).Length <= 0 then
            raise (GameBreakException(true))
        else
            this.RemoveDeadEntities ()
            this.CheckCollisions ()
            this.AdvanceEntities ()
    /// <summary>Check for collisions. This will remove colliding objects and add possible new ones (from the objects's collision handling methods)</summary>
    member this.CheckCollisions () =
        let mutable newEntities : Entity list = []
        let rec removeCollisions (entities : List<Entity>) : List<Entity> =
            match entities with
                | [] -> []
                | e::es ->
                    let collisions = es |> List.collect (fun e' -> 
                        match Entity.CheckCollision e e' with
                        | true ->
                            newEntities <- newEntities @ ([
                                (e.HandleCollision ());
                                (e'.HandleCollision ())
                            ] |> List.concat)
                            [e;e']
                        | false -> [])
                    if not (collisions |> (List.contains e)) then
                        e :: (removeCollisions (es |> List.filter (fun entity -> not (collisions |> List.contains entity))))
                    else
                        removeCollisions (es |> List.filter (fun entity -> not (collisions |> List.contains entity)))
        this.Entities <- ((removeCollisions this.Entities) @ newEntities)
    
    /// <summary>Remove entities that "ask" for it</summary>
    member this.RemoveDeadEntities () : unit = 
        this.Entities <- this.Entities |> List.filter (fun e -> not (e.ShouldDie ()))
    /// <summary>Advance all entities by a single timestep.</summary>
    member this.AdvanceEntities () : unit =
        this.Entities |> List.iter (fun e -> e.Advance this.TimeStepSize (this.Width, this.Height))
    /// <summary>Draw the current game state</summary>
    /// <param name="state">The game state</param>
    static member Draw (state: GameState) : Picture =
        state.update ()
        (emptyTree, state.Entities)
        ||> List.fold (fun acc e -> acc |> onto ((e :> IRenderable).Render()))
        |> make
        
    /// <summary>React to an event</summary>
    /// <param name="state">The game-state</param>
    /// <param name="event">The event</param>
    /// <returns>Some state if it is to be redrawn, otherwise None</returns>
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