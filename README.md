![Haskell CI](https://github.com/MartinErhardt/kell/actions/workflows/haskell.yml/badge.svg)
[![][license img]][license] 

# kell
This is a [POSIX](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html) shell written in Haskell.
## Try it out
Just type
```
cabal build
cabal test    # optional to run tests
cabal install
kell
```
## Features
The following features are implemented
- Line based Interpreter
  - parses a line and if it the parser fails with unexpected end of input tries to fetch an additional one
  - distinguish interactive mode (prompt and no exit on a large class of errors)
- Word expansions:
  - Parameter expansion
  - Command substition
  - Arithmetic expansion
- Variable assignments
- Input/Output redirections
- Execution of simple commands
- And-Or lists
- Asynchronous command execution (very basic only)
- Control structures:
  - If conditionals
  - While loops
- Some unit testing testing standard out and exit status of scripts against the shell in CI of Github

Not implemented, but soon to be:
- Tilde expansions
- More correct error handling
- Signal traps
- Built-in utilities
- Pattern matching
- Here-Documents
- Use a different string format like Bytestring or Text, that is not lazy and thus slow
- Source documentation with haddock

## Other
The shell can be crosscompiled to target the [Sortix operating system](https://sortix.org/) with this [ghc-toolchain](https://github.com/MartinErhardt/sortix-cross-ghc).

Please note, that you will need statically linked versions of gmp and iconv on your host-system for this to work.

[license]: LICENSE
[license img]: https://img.shields.io/badge/License-Apache%202-blue.svg
