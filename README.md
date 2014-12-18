# Deducers

A Clojure library designed to let you view data customized for a specific use; a new "view" of the data

## Usage

A minimal deducer acts like a nil-safe `let` block. Instead of an exception, it short-circuits:
```clojure
repl> (deduce
        [x 3
         y (+ x 4)]
        (* x y))
=> 21

repl> (deduce
        [x nil ;; <= lookers over here
         y (+ x 4)]
        (* x y))
=> nil
```

## License

Copyright © 2014 Paul Richardson

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
