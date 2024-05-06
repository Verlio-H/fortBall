A series focused lazily evaluated ball arithmetic library in fortran for arbitrary precision real number arithmetic. \
This library currently uses float128 from iso_fortran_env so it is limited in precision and also requires a fortran compiler with 128 bit float support. \
Converting to rationals is a future project\
**THIS PROJECT CURRENTLY DOES NOT RUN IN GFORTRAN** (I love buggy fortran compilers)\
It is known to work in ifort, all other fortran compilers are untested.\
\
Whats included: \
- Types:\
  - ball
    - A floating point value with an error bound, this is used for the result of computations and can also be used as inputs.
  - number
    - A tree that represents an expression, can be evaluated to a ball using the eval() function
    - This tree can also represent a function that can be differentiated and populated (see procedures)
    - Operating overloading exists for numbers so they act much like normal values
  - expandedint
    - An integer that is in the form a*n+b where a and b are integers and n is a summation index
- Procedures:
  - Construction:
    - arg(integer)
        - Constructs a number that refers to the nth argument to a function (starting from 0)
    - sum(expandedint start, expandedint end, number expr, number err, (optional) integer minn, (optional) real startval, (optional) real starterr)
        - Constructs a number representing the sum from start to end of the expression expr with error bound given by err
        - minn represents the minimum number of values calculated before calculating the error bound (useful if the error bound isn't correct for small values, or if there is derivatives involved)
        - startval and starterr form a ball representing the initial value of the sum which can be useful in error calculation
    - ssum(expandedint start, expandedint end, number expr, number err, (optional) integer minn, (optional) real startval, (optional) real starterr)
        - See sum
        - ssum (slow sum) represents a sum where the entire sum must be calculated before the error bound is known whereas sum represents a sum where the error bound is known as each term is added
        - these sums can be extremely slow in certain situations but are useful for definite integration
    - numb(integer), numb(expandedint)
        - Converts an integer or expandedint into a number so that it can be used to construct expressions
    - numb(real val, real err)
        - Constructs a number representing a ball with value val and error err.
    - eint(integer)
        - Constructs an expandedint from an integer
    - eintsum(integer)
        - Constructs an expandedint with a summation index where the summation index refers to the upwards depth of sum that it is referring to
        - For example if there was a double nested sum and the expandedint was in the central expression, eintsum(0) would refer to the inside sum index and eintsum(1) would refer to the outside sum index
    - factorial(integer), factorial(expandedint)
        - Constructs a number representing the factorial of the argument
  - Manipulation:
    - diff(number input, integer argument)
        - Differentiates a number with respect to the provided argument
    - populate(number input, number args(:))
        - Replaces each of the variables in the input with the number provided in the array (note that this number can be a function, so this function can be used for function composition and it works with functions that have been differentiated)
    - eval(number input, real eps)
        - Evaluates a number resulting in a ball
        - Attempts to make the error bound on the ball less than eps
        - Note that this function can result in infinite loops under some situations due to the usage of float128
  - Overloaded operations:
    
