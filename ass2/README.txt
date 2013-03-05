load 15-puzzle.lisp in the REPL, then run (main <depth-bound> <stats flag>).

My heuristic function, mis-match, computes how many tiles in a state are out of place (relative to the goal state).  It seems to do well except for the greedy algorithm on the hard problem.  As such, I have omitted that and other time-consuming searches from the main driver function's hard lineup.  Feel free to run these slowpokes separately by compiling the individual lines in 15-puzzle.lisp.

There isn't much more to say about my submission except that the operations are saved in nodes as actual functions (ergo #<FUNCTION WEST> rather than a string like "W").  The solution path should still be clear from this output.
