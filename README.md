asc2

Demonstrates how to build a component-based function

The function contains several async components that are run by the corresponding defuns. 

The child-components are contained as lambdas + routing-table + named-queues (one queue for
each child plus 1 for :self).

Each named-queue contains a 4-tuple {name x handler x input-queue x output-queue}.

The handler is a function that takes 1 arg - the message.

The system defines ports as 2-tuples { component x tag }.  A port can be connected to
  other ports of other components.
  
Container components that look like functions contain 1 input port and 1 output port.

The input port of a container is connected to children inputs.

The output port(s) of children can be connected to other children or to the container's output.

The dispatcher runs components until conclude is set to T.

At conclusion, the dispatcher returns and the container can do what it needs to do to return
a value to the caller.  Typically, the container accesses a free variable and returns its value.
Typically, child component(s) set the free variable and signal conclusion (by calling "concluded").

Note that not all components need to look like functions. Components can have more than 1 output (or less than 1).  Inputs and outputs are not guaranteed to occur simultaneously (synchronously).

This async structure mimics FP solutions, but, the async structure allows the Architect to better
state what is going on (in a human readable manner).


See the diagram in helloworld.drawio for helloworld and helloworld5.

The diagram is hand-compiled to (defun helloworld ...) and (defun helloworld5 ...).

This looks like "more code", but, most of it is boiler-plate and can be emitted by an async
language (DSL, HLL, SCN, whatever).  

The point of this exercise is to produce human-readable results that can be turned into HLLs.

Shrinking code size and "making the code fit on a T-shirt" does not always produce more human-readable results.

Optimizing code tends to discourage machine-generation (aka compilation) of code.

IMO, optimizing Architecture is more important than optimizing Code (sometimes called "beautifying" (beauty is only in the eye of the beholder ; what is beautiful to a researcher might not be so beautiful to a practitioner)). 
