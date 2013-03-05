load 15-puzzle.lisp in the REPL, then run (main <depth-bound> <stats flag>)

My heuristic function, mis-match, computes how many tiles in a state are out of place (relative to the goal state).

There isn't much more to say about my submission except that the operations are saved in nodes as actual functions (ergo #<FUNCTION WEST> rather than a string like "W").  The solution path should still be clear from this output.
