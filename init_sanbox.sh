#!/bin/bash
set -x #echo on
cabal sandbox init

cd hask-irc-core
cabal sandbox init --sandbox=../.cabal-sandbox
cd ..

cd hask-irc-runner
cabal sandbox init --sandbox=../.cabal-sandbox
cabal sandbox add-source ../hask-irc-core/
cd ..

cd hask-irc-core
cabal install -j3 --only-dependencies
cd ..
