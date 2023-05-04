# HaSOM - The Simple Object Machine implemented in HaSOM

## Introduction

SOM is a minimal Smalltalk dialect used to teach VM construction at the [Hasso
Plattner Institute][SOM]. It was originally built at the University of Århus
(Denmark) where it was also used for teaching.

A simple SOM Hello World looks like:

```Smalltalk
Hello = (
  run = (
    'Hello World!' println.
  )
)
```

This repository contains the Haskell implementation of SOM, including an
implementation of the SOM standard library and a number of examples. Please see
the [main project page][SOMst] for links to the VM implementations.

## Build and run

To build and run HaSOM, the [Haskell Tool Stack][Stack] is required. 

HaSOM can be built with:

    $ stack build

A simple Hello World program is executed with:

    $ stack run -- -c core-lib/Smalltalk/ -c core-lib/Examples/Hello.som Hello

For more options, run the help:

    $ stack run -- --help

## Tests

The tests can be executed with:

    $ stack run -- -c core-lib/Smalltalk/ -c core-lib/TestSuite/ TestHarness

Some of the tests terminates the VM. There is a modified test suite that excludes these
test, that can be ran with:

    $ stack run -- -c core-lib/Smalltalk/ -c core-lib/TestSuite/ -c TestSuite CustomTestHarness

Or with the helper script:

    $ ./TestSuite/run.sh


Documentation
------------

The documentation can be build with:
    
    $ stack haddock

It is then available from **.stack-work/dist/x86_64-linux/Cabal-3.4.1.0/doc/html/HaSOM/index.html**

 [SOM]: http://www.hpi.uni-potsdam.de/hirschfeld/projects/som/
 [SOMst]: https://som-st.github.io/
 [Stack]: https://docs.haskellstack.org/en/stable/
