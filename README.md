### Experimental meta-math-library for computer graphics

(being rewritten/refactored/etc, don't try to use it)

It is a "meta library" in the sense that it doesn't directly have
functions for the usual operations like matrix multiply. Instead it
has a dsl for defining 'abstract' versions of such operations (and
will probably eventually include libraries of various abstract
function definitions), and a compiler for generating concrete versions
to match a particular combination of data storage and
optimization/safety/etc requirements. It is mostly intended for
graphics (up to 4x4 matrices), so completely unrolls all
operations. It might eventually gain ability to handle large matrices
more reasonably, but existing libraries probably handle that better.

