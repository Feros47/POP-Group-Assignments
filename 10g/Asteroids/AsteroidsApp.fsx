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
type AsteroidsTests() =

    [<Test>]
    member this.NonOverlappingAsteroids() =
        // Arrange: Create asteroids with non-overlapping initial positions
        let asteroid1 = new Asteroid((10.0, 10.0), (5.0, 5.0), 10.0)
        let asteroid2 = new Asteroid((30.0, 30.0), (2.0, 2.0), 8.0)

        // Act & Assert
        asteroid1.Position |> should equal (10.0, 10.0)
        asteroid2.Position |> should equal (30.0, 30.0)

    [<Test>]
    member this.LegalVelocities() =
        // Arrange: Create asteroids with initial velocities ranging from 0 to 10 pixels per second
        let asteroid1 = new Asteroid((10.0, 10.0), (5.0, 5.0), 10.0)
        let asteroid2 = new Asteroid((20.0, 20.0), (0.0, 0.0), 8.0)

        // Act & Assert
        let (vx, vy) = asteroid1.Velocity
        let (vx', vy') = asteroid2.Velocity
        customAssertions.ShouldBeCloseTo(vx, 5.0, 0.001)
        customAssertions.ShouldBeCloseTo(vy, 5.0, 0.001)
        customAssertions.ShouldBeCloseTo(vx', 0.0, 0.001)
        customAssertions.ShouldBeCloseTo(vy', 0.0, 0.001)


    [<Test>]
    member this.NegativeVelocidityAsteroid() =
        // Arrange: Create an asteroid with negative velocity
        let asteroid = new Asteroid((20.0, 20.0), (-3.0, -3.0), 10.0)

        // Act & Assert
        asteroid.Velocity |> should equal (-3.0, -3.0)

    [<Test>]
    member this.NormalMovement() =
        // Arrange: Create an asteroid within Canvas boundaries
        let asteroid = new Asteroid((100.0, 100.0), (5.0, 5.0), 8.0)
        let gameState = new GameState((512, 512), 0.1)
        let mutable entities = gameState.Entities
        entities <- [asteroid]

        // Act: Simulate game advancement
        gameState.AdvanceEntities()

        // Assert: Check if the asteroid's position changes within Canvas boundaries
        let (vx, vy) = asteroid.Position
        customAssertions.ShouldBeCloseTo(vx,105.0, 105.0)
        customAssertions.ShouldBeCloseTo(vy,105.0, 105.0)

    [<Test>]
    member this.AsteroidWrapAround() =
        // Arrange: Create an asteroid moving towards Canvas boundaries
        let asteroid = new Asteroid((510.0, 510.0), (5.0, 5.0), 8.0)
        let gameState = new GameState((512, 512), 0.1)
        let mutable entities = gameState.Entities
        entities <- [asteroid]

        // Act: Simulate game advancement
        gameState.AdvanceEntities()

        // Assert: Check if the asteroid wraps around the Canvas
        let (vx, vy) = asteroid.Position
        customAssertions.ShouldBeCloseTo(vx, 2.0, 2.0)
        customAssertions.ShouldBeCloseTo(vy, 2.0, 2.0)

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

    testAndReport "Testing Non-Overlapping Asteroids" (AsteroidsTests().NonOverlappingAsteroids)
    testAndReport "Testing Legal Velocities" (AsteroidsTests().LegalVelocities)
    testAndReport "Testing Negative Velocidity" (AsteroidsTests().NegativeVelocidityAsteroid)
    testAndReport "Testing Normal Movement Asteroids" (AsteroidsTests().NormalMovement)
    testAndReport "Testing Asteroid Wrap Around" (AsteroidsTests().AsteroidWrapAround)


    if allTestsPassed then
        printfn "All tests passed."
    else
        printfn "Some tests failed."



[<EntryPoint>]
let main (args : string array) : int =
    let gs = new GameState((512,512), 0.1)
    gs.Run ()