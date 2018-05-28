# Grover's Search Algorithm

Given a classical function f(x), where for some value a, f(a) = 1, and for all
other values x f(x) = 0, find a.

For finite search space, this is pretty easy in classical computation: you can
achieve O(n) time just by searching each enumerated value.  Grover's algorithm
however lets you find it in O(sqrt(n)) time using quantum computation.

_**Note:** constant factors on qubit states are omitted for notational
convenience.  The norm of any given qubit state is 1, so for example
"|0> + |1>" would actually mean 1/sqrt(2) (|0> + |1>)_

## Reminder: Unitary Operators

In quantum computation, an operation must be **unitary** to not collapse the
wavefunction.  An operator is unitary if the product of the operator and its
adjoint is equal to the identity operator:

A† = (A*)<sup>T</sup>  -- The adjoint is the transpose of the complex conjugate  
AA† = A†A = I -- To be unitary, the product must equal I 

## The Oracle

Consider the operator |x>|y> -> |x>|f(x)>.  This is not invertible - we have no
idea what value y had by looking at the output, and so the operator also can't
be unitary.  Instead we use an ancilla qubit, and apply the controlled not to
it rather than replacing it with the function result: |x>|y> -> |x>|y ⊕ f(x)>.
This is indeed invertible.

For the purposes of Grover's Search Algorithm, we shall use an oracle defined
as above, but where |x> represents a register of n qubits, and |y> represents
a single ancilla qubit.  By convention, this oracle is called U<sub>f</sub>.

## Phase shift

When you set |y> = |0> in the oracle, you get |x>|f(x)> out.  But what about when you
set |y> = |0> - |1>?  When f(x) = 0, you get |x>(|0> - |1>); when f(x) = 1, you get
|x>(|1> - |0>).  You can see this for yourself by distributing the CNOT over the
subtraction.

For this problem, when f(x) = 1 only when x = a, we can use y = |0> - |1> to get
a conditional phase shift: when x = a, U<sub>f</sub>|xy> = -|xy>; otherwise
U<sub>f</sub>|xy> = |xy>.
