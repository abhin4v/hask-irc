#!/bin/bash
set -x #echo on
cabal sandbox init

cd hask-irc-core
cabal sandbox init --sandbox=../.cabal-sandbox
cd ..

cd hask-irc-handlers
cabal sandbox init --sandbox=../.cabal-sandbox
cabal sandbox add-source ../hask-irc-core/
cd ..

cd hask-irc-runner
cabal sandbox init --sandbox=../.cabal-sandbox
cabal sandbox add-source ../hask-irc-core/
cabal sandbox add-source ../hask-irc-handlers/
cd ..
