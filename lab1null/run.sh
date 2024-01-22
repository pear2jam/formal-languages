#!/bin/sh
ghc main.hs
./main
z3 -smt2 out.smt2
