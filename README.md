# Grover's Search Algorithm

Given a classical function f(x), where for some value a, f(a) = 1, and for all
other values x f(x) = 0, find a.

For finite search space, this is pretty easy in classical computation: you can
achieve O(n) time just by searching each enumerated value.  Grover's algorithm
however lets you find it in O(sqrt(n)) time using quantum computation.

## The Oracle

In quantum computation, an operation must be **unitary** to not collapse the
wavefunction.  An operator is unitary if the product of the operator and its
adjoint is equal to the identity operator:

A^✝ = (A*)^T  -- The adjoint is the transpose of the complex conjugate
AA^✝ = A^(✝)A = I -- To be unitary, the product must equal I 
