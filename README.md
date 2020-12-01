# Chez Dynamic

*chez-dynamic* is a collection of functions / syntax (macros) that aid in dynamic development when using *Chez* scheme.

## Install and use

The supplied GNU Makefile will install to *LIBDIR*. The default location is *~/lib/csv<CHEZ-SCHEME-VERSION>*.

*LIBDIR* must be in **(library-directories)** to be usable from *Chez scheme*.

eg,
```
$ make install
```

To override LIBDIR:

```
$ make LIBDIR=~/newlibdir install
```

To use from *Chez* scheme:

```scheme
(import (dynamic))
```

## reimport

**reimport** allows for reloading R6RS libraries in *Chez* scheme.

It is designed for use within the *Chez* REPL. Use in any other environments will involve adjusting the **(current-environment)** parameter.

**reimport** uses **(library-search-handler)** to find the source file *only* for a library. Future versions may include the ability to load shared object files via **(load-shared-object)**.

Note that **reimport** uses **eval** to refresh bindings and must be used with caution. Please study the source code if using in public works.

### Example

Create a library **x** such that it exists within **(library-directories)**:
```scheme
(library (x)
  (export val)
  (import (chezscheme))
 (define val 3))
```

Now import into a *Chez* repl:
```scheme
> (import (x))
```

Edit the **val** define in the **x** library:

```scheme
(define val 4)
```

And in the *Chez* REPL, reimport to see the new value:
```scheme
> (reimport (x))
> val
4
```

### Beware

**reimport** uses **eval** to recreate library bindings and so must be used with caution. It does that within **current-environment**.

## case-import

Perform an import and subsequent defines based on the existance of a library.

Note that this function only checks for the existance of the library, and not whether it imports successfully.

### example

This example shows how **case-import** could be used to define a set of bindings based on whether *irregex* or *srfi :115* regexp bindings are available.

```scheme
(case-import
 [(irregex)
 ...]
 [(srfi :115 regexp)
 ...])
```

## current-environment

A parameter that defaults to **interaction-environment**. This is used by the **eval** in **reimport**.

## License

Written by Akce and released into the public domain.
