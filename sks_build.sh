#!/bin/bash

# SKS build script.
# cd to directory with "dump" subdirectory, and run
# You might want to edit this file to reduce or increase memory usage 
# depending on your system

fail() { echo Command failed unexpectedly.  Bailing out; exit -1; }

echo === Running fastbuild... ===
if ! sks fastbuild -n 10 -cache 100; then fail; fi
echo === Cleaning key database... ===
if ! sks cleandb; then fail; fi
echo === Building ptree database... ===
if ! sks pbuild -cache 20 -ptree_cache 70; then fail; fi
echo === Done! ===
