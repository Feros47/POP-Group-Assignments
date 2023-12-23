#load "Asteroids.fs"
#r "nuget:DIKU.Canvas, 2.0.2"
#r "nuget:FsUnit, 3.0.0"
open Asteroids
open Canvas
open NUnit.Framework
open FsUnit
open System



type CustomGameAssertions() =
    member this.ShouldBeCloseTo(value : float, expected, tolerance) =
        Assert.That(value, Is.InRange(expected - tolerance, expected + tolerance))

let customAssertions = CustomGameAssertions()



[<TestFixture>]
type SpaceshipTests() =

    [<Test>]
    member this.SpaceshipInitialization() =
        let spaceship = new Spaceship((10.0, 10.0), (1.0, 0.0), 10.0)
        let length (x,y) = sqrt(x*x + y*y)
        let parrallel = (1.0,0.0 / (length(1.0,0.0)))

        spaceship.Position |> should equal (10.0, 10.0)
        spaceship.Direction |> should equal (1.0,0.0)
        spaceship.Velocity |> should equal (0.0, 0.0)
        spaceship.Direction |> should equal parrallel

    [<Test>]
    member this.SpaceshipRotation() =
        let spaceship = new Spaceship((10.0, 10.0), (1.0, 0.0), 10.0)
        spaceship.Rotate Clockwise
        Assert.False (spaceship.Direction = (1.0,0.0))
        spaceship.Rotate CounterClockwise
        spaceship.Direction |> should equal (1.0, 0.0)
        spaceship.Velocity |> should equal (0.0, 0.0)
        
    [<Test>]
    member this.SpaceshipAcceleration() =
        let spaceship = new Spaceship((10.0, 10.0), (1.0, 0.0), 10.0)
        spaceship.Accelerate 2.0
        Assert.True (spaceship.Velocity <> (0.0,0.0))
        spaceship.Brake 2.0
        spaceship.Velocity |> should equal (0.0, 0.0)

    [<Test>]
    member this.SpaceshipMaxVelocity() =
        let length a = sqrt ((fst a) ** 2.0 + (snd a) ** 2.0)
        let spaceship = new Spaceship((10.0, 10.0), (1.0, 0.0), 5.0)
        let mutable vPrev = spaceship.Velocity
        spaceship.Accelerate 0.1
        let mutable vCurr = spaceship.Velocity
        while (length vPrev) < (length vCurr) do
            vPrev <- spaceship.Velocity
            spaceship.Accelerate 0.1
            vCurr <- spaceship.Velocity
        (length vCurr) |> should equal 20.0

    [<Test>]
    member this.SpaceshipCollision() =
        let spaceship = new Spaceship((10.0, 10.0), (1.0, 0.0), 5.0)
        try
            ignore (spaceship.HandleCollision ())
            Assert.False (true)
        with GameBreakException e -> Assert.True (not e)



    [<Test>]
    member this.SpaceshipBullet() =
        let spaceship = new Spaceship((10.0, 10.0), (1.0, 0.0), 10.0)
        let bullet = spaceship.MakeBullet()
        bullet.Position |> should equal (40.0, 10.0)
        bullet.Velocity |> should equal (21.0, 0.0)

    [<Test>]
    member this.BulletFromMovingSpaceship() =
        let spaceship = new Spaceship((10.0,10.0), (1.0,0.0), 10.0)
        // Make spaceship move at 10 px / s
        spaceship.Accelerate(1)
        let bullet = spaceship.MakeBullet ()
        bullet.Position |> should equal (40.0, 10.0)
        bullet.Velocity |> should equal (30.0, 0.0)



type EntityTests() =

    [<Test>]
    member this.NormalMovement() =
        let ss = new Spaceship((256.0,256.0), (5.0,5.0),0.14)
        ss.Accelerate (1.0)
        let entities : Entity list = [
            new Asteroid((256.0,256.0), (5.0,5.0), 32.0);
            ss;
            new Bullet((256.0, 256.0), (5.0, 5.0))]
        Assert.True (entities |> List.forall (fun e -> this.hasNormalMovement e))
    [<Test>]
    member this.EntityWraparound() =
        let ss = new Spaceship((511.0,511.0), (5.0,5.0),50.0)
        ss.Accelerate (1.0)
        let entities : Entity list = [
            new Asteroid((510.0,510.0), (5.0,5.0), 32.0);
            ss;
            new Bullet((510.0, 510.0), (5.0, 5.0))]
        Assert.True (entities |> List.forall (fun e -> this.hasNormalMovement e))

    member this.hasNormalMovement (entity : Entity) : bool =
        let position = entity.Position
        let velocity = entity.Velocity
        entity.Advance 1.0 (512,512)

        let xExpected = ((fst position) + (fst velocity) + 512.0) % 512.0
        let yExpected = ((snd position) + (snd velocity) + 512.0) % 512.0
        printfn "%A" entity.Position
        entity.Position = (xExpected, yExpected)

type AsteroidsTests() =


    [<Test>]
    member this.LegalVelocities() =
        let asteroid1 = new Asteroid((10.0, 10.0), (5.0, 5.0), 10.0)

        let (vx, vy) = asteroid1.Velocity
        customAssertions.ShouldBeCloseTo(vx, 5.0, 0.1)
        customAssertions.ShouldBeCloseTo(vy, 5.0, 0.1)
    [<Test>]
    member this.NegativeVelocityAsteroid() =
        let asteroid = new Asteroid((20.0, 20.0), (-3.0, -3.0), 10.0)

        asteroid.Velocity |> should equal (-3.0, -3.0)




    [<Test>]
    member this.LargeAsteroidCollison() =
        let asteroid = new Asteroid((256.0,256.0), (1.0,1.0), 9.0)
        let result = asteroid.HandleCollision ()

        Assert.True (
            result.Length = 2 &&
            result.Head.Radius = 9.0 / 2.0 &&
            (result |> List.last).Radius = 9.0 / 2.0)
    
    [<Test>]
    member this.SmallAsteroidCollision() =
        let asteroid = new Asteroid((256.0,256.0), (1.0,1.0), 8.0)
        let result = asteroid.HandleCollision ()
        Assert.True ((result = []))


type BulletTests() =

    [<Test>]
    member this.BulletCollision() =
        let bullet = new Bullet((100.0, 100.0), (5.0,5.0))
        (bullet.HandleCollision ()) |> should equal []

    [<Test>]
    member this.BulletLifespan() =
        let bullet = new Bullet((0.0,0.0), (0.0,0.0))
        let start = DateTime.Now
        while not (bullet.ShouldDie ()) do
            Async.Sleep 100 |> ignore
        let tEnd = DateTime.Now
        customAssertions.ShouldBeCloseTo (float (tEnd - start).TotalMilliseconds, 2000, 25)
    
type GameStateTests() =
    
    [<Test>]
    member this.TestGameInitialization() =
        let gamestate = new GameState((512,512), 10, 0.1, false)
        let asteroids = (gamestate.Entities |> List.filter (fun e -> (e.GetType() = typeof<Asteroid>)))
        let spaceships = (gamestate.Entities |> List.filter (fun e -> (e.GetType() = typeof<Spaceship>)))
        gamestate.Entities.Length |> should equal 11
        asteroids.Length |> should equal 10
        spaceships.Length |> should equal 1

    [<Test>]
    member this.TestGameStartNoCollisions() =
        let gamestate = new GameState((512,512), 10, 0.1, false)
        let currentEntities = gamestate.Entities
        gamestate.CheckCollisions ()
        currentEntities |> should equal gamestate.Entities
        



let runTests () =
    let mutable allTestsPassed = true

    let testAndReport testName testFunction =
        try
            testFunction ()
            printfn "%s: Passed" testName
        with
        | _ ->
            printfn "%s: Failed" testName
            allTestsPassed <- false

    // Entity tests
    testAndReport "Testing Entity Normal Movement" (EntityTests().NormalMovement)
    testAndReport "Testing Entity Wrap Around" (EntityTests().EntityWraparound)

    // Asteroid tests
    testAndReport "Testing Asteroid Legal Velocities" (AsteroidsTests().LegalVelocities)
    testAndReport "Testing Asteroid Negative Velocidity" (AsteroidsTests().NegativeVelocityAsteroid)
    testAndReport "Testing Large Asteroid Collision Handling" (AsteroidsTests().LargeAsteroidCollison)
    testAndReport "Testing Small Asteroid Collision Handling" (AsteroidsTests().SmallAsteroidCollision)

    // Spaceship tests
    testAndReport "Testing Spaceship Initialization" (SpaceshipTests().SpaceshipInitialization)
    testAndReport "Testing Spaceship Rotation" (SpaceshipTests().SpaceshipRotation)
    testAndReport "Testing Still Standing Spaceship Shooting Bullet" (SpaceshipTests().SpaceshipBullet)
    testAndReport "Testing Moving Spaceship Shooting Bullet" (SpaceshipTests().BulletFromMovingSpaceship)
    testAndReport "Testing Spaceship Acceleration and Braking" (SpaceshipTests().SpaceshipAcceleration)
    testAndReport "Testing Maximum Spaceship Velocity" (SpaceshipTests().SpaceshipMaxVelocity)
    testAndReport "Testing Spaceship Collision Leads To Game Break Exception" (SpaceshipTests().SpaceshipCollision)

    // Bullet tests
    testAndReport "Testing Bullet Collision" (BulletTests().BulletCollision)
    testAndReport "Testing Bullet Lifespan (might be slow)" (BulletTests().BulletLifespan)

    // GameState tests
    testAndReport "Testing Number Of Entities in Initial GameState" (GameStateTests().TestGameInitialization)
    testAndReport "Testing For Collisions in Initial GameState" (GameStateTests().TestGameStartNoCollisions)


    if allTestsPassed then
        printfn "All tests passed."
    else
        printfn "Some tests failed."



[<EntryPoint>]
let main (args : string array) : int =
    runTests()
    let gs = new GameState((512,512), 10, 0.1, false)
    gs.Run ()