# <p style="text-align: center;"> Block Dude 2021</p>

Project developed for the subject "IT Labs I" written in Haskell, using [Gloss](https://hackage.haskell.org/package/gloss) for the 2D Graphics. This is a recreation of the original [Block Dude](https://azich.org/blockdude/) game.

## Additional Haskell libraries

The project runs with some external haskell libraries.

```bash
$ cabal update
```

#### Gloss

```bash
$ cabal install --lib gloss
```
#### Random
```bash
$ cabal install --lib random
```

## Unit Tests

This project contains unit tests written using the [HUnit](https://hackage.haskell.org/package/HUnit) library. Tests can be run as follows:

```bash
$ ghci -i="src" -i="tests" tests/Tests.hs
>>> runTestsT1 -- Run task 1 tests
>>> runTestsT2 -- Run task 2 tests
>>> runTestsT3 -- Run task 3 tests
>>> runTestsT4 -- Run task 4 tests
>>> runTestsT6 -- Run task 5 tests
>>> runAllTests -- Run all tests
```
