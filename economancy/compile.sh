#!/bin/bash

stack build economancy
cp .stack-work/dist/x86_64-linux/ghc-9.6.4/build/economancy/economancy .
./economancy-x86_64-linux/bin/game economancy economancy-x86_64-linux/bin/play_random
