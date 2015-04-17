## 0.3.2

* added `to-chan`
* added `select-step`, like `reductions` but with multiple source signals that each get their own reducing function 
* flipped argument order of the `setup!` function passed to `splice` 
* added `indexed-updates`, a signal transformer useful for performing different effects based on which source signal has updated
* added `reductions` function, which works like `reducep` but has a better name and doesn't do an implicit `drop-repeats`
* removed `reducep` and `transducep`
* got rid of `j.z.i.signal/output-mult`
* calling core.async's `tap` function on a live graph created with `spawn` now supplies you with straight fresh values from the output signal, instead of batches of fresh & cached messages
  - this means that values of an output signal can never be `nil`, since core.async channels don't do `nil`s
* added `write-port`, a signal constructor that lets you treat the returned signal like a write-only `core.async` channel.

## 0.3.1

* fixed a bug that would throw an error if you tried to use a folding signal like `foldp` on a signal that emitted multiple values from a single event, e.g. a `pipeline` with an "expanding" transducer like `cat`

## 0.3.0

* `lift`/`liftseq` renamed to `map`/`mapseq`
* `input` signal constructor now also accepts a channel or mult as its value source
* new `pipeline` function lets you transform signals using transducers
* extracted some implementation details into `impl` namespaces
* `splice` function lets you create arbitrary asynchronous signal transformations
* a bunch of new functions in the `time` namespace (`timestamp`, `delay`, `since`, `debounce`)
* `impl.signal/signal-mult` function to get a particular signal's message mult from a live graph
* signals now provide init functions to calculate their initial values whenever a live graph is fired up
* various functions which previously required a base value now have lesser-arity forms which inherit the initial value of one of their parent signals
* `activate-when` function is like `keep-when`, but entirely deactivates unused signals
