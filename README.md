## Description

https://adventofcode.com/2017

## Regular expressions in Haskell

* brew install pcre
* cabal install regex-pcre

## TODO

### Missing Haskell implementations

On some days, I only did a JavaScript implementation.

* Day12, Day13, Day19, Day20, Day21, Day22

### Parsing input files

Use Parsec to parse input files ?

* [Intro to Parsing with Parsec in Haskell](https://jakewheat.github.io/intro_to_parsing)

### Making the Haskell programs go faster

On some days, I have both JavaScript and Haskell implementations. In a few cases, the JavaScript is much faster due to in-place mutation of collections/objects etc. How can I improve the Haskell implementations so that they have comparable speed ?

* Day16, Day17, Day25
* [Haskell Performance Resource](https://wiki.haskell.org/Performance)
* [Haskell High Performance Programming](https://www.packtpub.com/application-development/haskell-high-performance-programming)
* [Tutorial: Profiling Cabal projects](https://nikita-volkov.github.io/profiling-cabal-projects/)

```
cabal configure --enable-profiling --enable-tests --enable-benchmarks
```

