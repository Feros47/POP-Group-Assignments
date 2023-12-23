#load "Asteroids.fs"
#r "nuget:DIKU.Canvas, 2.0.2"
#r "nuget:FsUnit, 3.0.0"
open Asteroids
open Canvas
open NUnit.Framework
open FsUnit




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
        spaceship.Rotate CounterClockwise
        spaceship.Direction |> should equal (1.0, 0.0)
        spaceship.Velocity |> should equal (0.0, 0.0)
        
    [<Test>]
    member this.SpaceshipMovement() =
        let spaceship = new Spaceship((10.0, 10.0), (1.0, 0.0), 10.0)
        spaceship.Accelerate 2.0
        spaceship.Brake 2.0
        spaceship.Velocity |> should equal (0.0, 0.0)

    [<Test>]
    member this.SpaceshipBullet() =
        let spaceship = new Spaceship((10.0, 10.0), (1.0, 0.0), 10.0)
        let bullet = spaceship.MakeBullet()
        bullet.Position |> should equal (40.0, 10.0)
        bullet.Velocity |> should equal (21.0, 0.0)




type AsteroidsTests() =

    [<Test>]
    member this.NonOverlappingAsteroids() =
        let asteroid1 = new Asteroid((10.0, 10.0), (5.0, 5.0), 10.0)
        let asteroid2 = new Asteroid((30.0, 30.0), (2.0, 2.0), 8.0)

        asteroid1.Position |> should equal (10.0, 10.0)
        asteroid2.Position |> should equal (30.0, 30.0)

    [<Test>]
    member this.LegalVelocities() =
        let asteroid1 = new Asteroid((10.0, 10.0), (5.0, 5.0), 10.0)
        let asteroid2 = new Asteroid((20.0, 20.0), (0.0, 0.0), 8.0)

        let (vx, vy) = asteroid1.Velocity
        let (vx', vy') = asteroid2.Velocity
        customAssertions.ShouldBeCloseTo(vx, 5.0, 0.001)
        customAssertions.ShouldBeCloseTo(vy, 5.0, 0.001)
        customAssertions.ShouldBeCloseTo(vx', 0.0, 0.001)
        customAssertions.ShouldBeCloseTo(vy', 0.0, 0.001)


    [<Test>]
    member this.NegativeVelocidityAsteroid() =
        let asteroid = new Asteroid((20.0, 20.0), (-3.0, -3.0), 10.0)

        asteroid.Velocity |> should equal (-3.0, -3.0)

    [<Test>]
    member this.NormalMovement() =
        let asteroid = new Asteroid((100.0, 100.0), (5.0, 5.0), 8.0)
        let gameState = new GameState((512, 512), 0.1)
        let mutable entities = gameState.Entities
        entities <- [asteroid]

        //gameState.AdvanceEntities()

        let (vx, vy) = asteroid.Position
        customAssertions.ShouldBeCloseTo(vx,105.0, 2)
        customAssertions.ShouldBeCloseTo(vy,105.0, 2)

    [<Test>]
    member this.AsteroidWrapAround() =
        let asteroid = new Asteroid((510.0, 510.0), (5.0, 5.0), 8.0)
        let gameState = new GameState((512, 512), 0.1)
        let mutable entities = gameState.Entities
        entities <- [asteroid]

        //gameState.AdvanceEntities()

        let (vx, vy) = asteroid.Position
        customAssertions.ShouldBeCloseTo(vx, 2.0, 2.0)
        customAssertions.ShouldBeCloseTo(vy, 2.0, 2.0)

type BulletTests() =

    [<Test>]
    member this.BulletMovement() =
        let bullet = new Bullet((100.0, 100.0), (5.0, 5.0))
        let gameState = new GameState((512, 512), 0.1)
        let mutable entities = gameState.Entities
        entities <- [bullet]

        //gameState.AdvanceEntities()

        let (vx, vy) = bullet.Position
        customAssertions.ShouldBeCloseTo(vx, 105.0, 2.0)
        customAssertions.ShouldBeCloseTo(vy, 105.0, 2.0)

    [<Test>]
    member this.BulletWrapAround() =
        let bullet = new Bullet((510.0, 510.0), (5.0, 5.0))
        let gameState = new GameState((512, 512), 0.1)
        let mutable entities = gameState.Entities
        entities <- [bullet]

        //gameState.AdvanceEntities()

        let (vx, vy) = bullet.Position
        customAssertions.ShouldBeCloseTo(vx, 2.0, 2.0)
        customAssertions.ShouldBeCloseTo(vy, 2.0, 2.0)
    
type CollisionsTests() =
    
    [<Test>]
    member this.CollisionSpaceshipAsteroid() =
        let spaceship = new Spaceship((100.0, 100.0), (1.0, 0.0), 10.0)
        let asteroid = new Asteroid((100.0, 100.0), (5.0, 5.0), 8.0)
        let gameState = new GameState((512, 512), 0.1)
        let mutable entities = gameState.Entities
        entities <- [spaceship; asteroid]
    
        //gameState.AdvanceEntities()
    
        
    
    [<Test>]
    member this.CollisionBulletAsteroid() =
        let bullet = new Bullet((100.0, 100.0), (5.0, 5.0))
        let asteroid = new Asteroid((100.0, 100.0), (5.0, 5.0), 8.0)
        let gameState = new GameState((512, 512), 0.1)
        let mutable entities = gameState.Entities
        entities <- [bullet; asteroid]
    
        //gameState.AdvanceEntities()
        


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

    testAndReport "Testing Spaceship Initialization" (SpaceshipTests().SpaceshipInitialization)
    testAndReport "Testing Spaceship Rotation" (SpaceshipTests().SpaceshipRotation)
    testAndReport "Testing Spaceship Movement" (SpaceshipTests().SpaceshipMovement)
    testAndReport "Testing Spaceship Bullet" (SpaceshipTests().SpaceshipBullet)
    testAndReport "Testing Non-Overlapping Asteroids" (AsteroidsTests().NonOverlappingAsteroids)
    testAndReport "Testing Legal Velocities" (AsteroidsTests().LegalVelocities)
    testAndReport "Testing Negative Velocidity" (AsteroidsTests().NegativeVelocidityAsteroid)
    testAndReport "Testing Normal Movement Asteroids" (AsteroidsTests().NormalMovement)
    testAndReport "Testing Asteroid Wrap Around" (AsteroidsTests().AsteroidWrapAround)
    testAndReport "Testing Bullet Movement" (BulletTests().BulletMovement)
    testAndReport "Testing Bullet Wrap Around" (BulletTests().BulletWrapAround)
    testAndReport "Testing Collision Spaceship Asteroid" (CollisionsTests().CollisionSpaceshipAsteroid)
    testAndReport "Testing Collision Bullet Asteroid" (CollisionsTests().CollisionBulletAsteroid)




    if allTestsPassed then
        printfn "All tests passed."
    else
        printfn "Some tests failed."



[<EntryPoint>]
let main (args : string array) : int =
    runTests()
    let gs = new GameState((512,512), 0.1)
    gs.Run ()