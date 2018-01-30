#!/bin/bash
## used to generate test data from Godot.
## all the gd scripts in the gdscript directory are run which output a binary file into priv/gdscript_bin
##
## Need to have a local copy of Godot to run this.

if [ -z "$GODOT_BINARY" ]; then
    echo "Set environment variable GODOT_BINARY"
    exit 1
fi

for f in gdscript/*
do
    eval "$GODOT_BINARY -s $f"
done
