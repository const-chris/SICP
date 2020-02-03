#lang sicp

#|
The hint gives away the main point: if we need to acquire a shared resource in order to determine what further shared resources we need, we can't know ahead of time in what order to acquire all the resources our procedure will need, and deadlock becomes a possibility.

One fairly contrived example of this problem: Suppose we want to devise a system where each object in the system maintains some local state value that is mutated by observing it--say that the state is maintained by a pseudorandom generator whose algorithm is a secret from the rest of the system. Now suppose we are using this system to simulate some behavior, and as part of that simulation, we need a procedure on the system which looks at an object and exchanges its value for that of a second object whose id matches said value. We cannot release and reacquire the first object because its state would change the next time we observe it. If two objects happened simultaneously each to have a value that matched the other's id, this would create a deadlock that could not be avoided by deciding which to acquire first based on id.
|#
