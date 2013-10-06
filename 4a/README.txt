Tyrone Hinderson
tph2107
Assignment #4

I didn't use anything from the AIMA code. Any similarities therewith are flattering coincidences.  My driver macroreads a file filled with training data, devises a decision tree for the data, then returns a function; the function takes example data as input and produces classifications (according to the dt) as output.  After compiling everything in dtl.lisp, use this format in the repl:

(funcall (learn <training_data_filename>) <example_data>)

e.g.
(funcall (learn "restaurantlsp.txt") '(Yes No No Yes Some $$$ No Yes French 0-10 Yes))
will return YES
