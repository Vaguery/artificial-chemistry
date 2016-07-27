# artificial-chemistry

See [this "learning in public" sequence](http://vaguery.com/words/an-artificial-chemistry) for more information.

This is a quick pass through Wolfgang Banzhaf and Christian Lasarczyk's paper ["Genetic Programming of an Artificial Chemistry"](http://www.cs.mun.ca/~banzhaf/chapters.html#Genetic%20Programming%20of%20an%20Algorithmic%20Chemistry), from the 2004 Genetic Programming Theory and Practice workshop in Ann Arbor, MI.

I'm working in Clojure, but there's no particular reason for that except it's on hand.

## What it does

I implement a `RegisterMachine` system in Clojure, based on the description in the original paper, and then write a brute-force evolutionary algorithm or two to search for classifier algorithms (using the same UCI data set cited originally) and a symbolic regression system that approximates `y=sin(x)` with non-transcendental primitives.


## How to run the tests

The project uses [Midje](https://github.com/marick/Midje/).

`lein midje` will run all tests.

`lein midje namespace.*` will run only tests beginning with "namespace.".

`lein midje :autotest` will run all the tests indefinitely. It sets up a
watcher on the code files. If they change, only the relevant tests will be
run again.
