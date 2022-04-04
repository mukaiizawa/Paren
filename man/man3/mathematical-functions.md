# NAME
sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, exp, log, pow, sqrt - mathematical functions.

# SYNOPSIS

    (sin X)
    (cos X)
    (tan X)
    (asin X)
    (acos X)
    (atan X)
    (sinh X)
    (cosh X)
    (tanh X)
    (exp X)
    (log X :opt Y)
    (pow X)
    (sqrt X)

# DESCRIPTION
These functions are basic mathematical functions.

# RETURN VALUE
## Trigonometric functions
The function `sin` returns the sine of `X`, where `X` is given in radians.

The function `cos` returns the cosine of `X`, where `X` is given in radians.

The function `tan` returns the tangent of `X`, where `X` is given in radians.

## Inverse trigonometric functions
The function `asin` returns the principal value of the arc sine of `X` in radians; the return value is in the range `[-pi/2, pi/2]`.

The function `acos` returns the arc cosine of `X` in radians; the return value is in the range `[0, pi]`.

The function `atan` returns the principal value of the arc tangent of `X` in radians; the return value is in the range `[-pi/2, pi/2]`.

## Hyperbolic functions
The function `sinh` returns the hyperbolic sine of `X`.

The function `cosh` returns the hyperbolic cosine of `X`.

The function `tanh` returns the hyperbolic tangent of an angle `X`.

    sinh(x) = (exp(x) - exp(-x)) / 2
    cosh(x) = (exp(x) + exp(-x)) / 2
    tanh(x) = sinh(x) / cosh(x)

## Exponential and Logarithmic functions
The function `exp` returns the Euler's number `e` raised to the power of `X`.

The function `log` returns the base `X` logarithm of `Y`. If `Y` is omitted, the natural logarithm of `X` is returned.

The function `pow` returns the value of `X` raised to the power of `Y`.

The function `sqrt` returns  the square root of `X`.

# NOTES
These functions call the C standard math library.

# ERRORS
These functions error if result is not finite.

# EXAMPLES

    ) (sin 0)
    0
    ) (cos 0)
    1
    ) (tan 0)
    0

    ) (asin (sin 0))
    0
    ) (acos (cos 0))
    0
    ) (atan (tan 0))
    0

    ) (= (sinh 1) (/ (- (exp 1) (exp -1)) 2))
    true
    ) (= (cosh 1) (/ (+ (exp 1) (exp -1)) 2))
    true
    ) (= (tanh 1) (/ (sinh 1) (cosh 1)))
    true

    ) (= (log (exp 10)) 10))
    true
    ) (= (log (pow 2 10)) (* 10 (log 2)))
    true
    ) (= (log 10 100) (/ (log 100) (log 10))))
    true
    ) (= (// (pow 2 10)) 1024))
    true
    ) (= (sqrt (pow 25 2)) 25))
    true

# SEE ALSO
- `abs(3)`
