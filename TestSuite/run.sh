#!/bin/bash

stack run -- exec \
    -c core-lib/Smalltalk/ \
    -c core-lib/TestSuite/ \
    -c TestSuite \
    --time \
    CustomTestHarness $@
