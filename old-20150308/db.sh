#!/bin/sh
cabal build
EXE=dist/build/otot/otot
#$EXE add type=list "title=Administrative IT Tasks" label=admin
$EXE add type=list "title=Family" label=family
$EXE add type=list "title=Questions" label=questions parent=family
