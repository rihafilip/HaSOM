#!/bin/bash

stack run -- exec \
    -c core-lib/Smalltalk/ \
    -c core-lib/TestSuite/ \
    -c CustomTestHarness.som \
    CustomTestHarness
