## 0.3.0

### Changes

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
