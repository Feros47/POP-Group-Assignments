module Cellular

// grid positions, must be within grid size
type Pos = int*int
// grid sizes, must be nonnegative
type Size = Pos
// Automaton states: Maps from grid positions to their values
type State<'a> = Map<Pos, 'a>
// Rules: functions mapping current cell and neighbourhood values
// to new cell value
type Rule<'a> = 'a -> 'a list -> 'a
// Updates: functions mapping old state to new state
type Update<'a> = State<'a> -> State<'a>
// life-like cellular automaton
val cellularAutomaton :
    Size * Rule<'a>     // size and update rule
    -> Update<'a>       // Update function.
