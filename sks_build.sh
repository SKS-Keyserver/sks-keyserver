#!/bin/bash

# SKS build script.
# cd to directory with "dump" subdirectory, and run
# You might want to edit this file to reduce or increase memory usage
# depending on your system

trap ignore_signal USR1 USR2

ignore_signal() {
    echo "Caught user signal 1 or 2, ignoring"
}

ask_mode() {
    echo "Please select the mode in which you want to import the keydump:"
    echo ""
    echo "1 - fastbuild"
    echo "    only an index of the keydump is created and the keydump cannot be"
    echo "    removed."
    echo ""
    echo "2 - normalbuild"
    echo ""
    echo "    all the keydump will be imported in a new database. It takes longer"
    echo "    time and more disk space, but the server will run faster (depending"
    echo "    from the source/age of the keydump)."
    echo "    The keydump can be removed after the import."
    echo ""
    echo -n "Enter enter the mode (1/2): "
    read
    case "$REPLY" in
     1)
        mode="fastbuild"
     ;;
     2)
        mode="build /var/lib/sks/dump/*.pgp"
     ;;
     *)
        echo "Option unknown. bye!"
        exit 1
     ;;
    esac
}

fail() { echo Command failed unexpectedly.  Bailing out; exit -1; }

ask_mode

echo "=== Running (fast)build... ==="
if ! /usr/sbin/sks $mode -n 10 -cache 100; then fail; fi
echo === Cleaning key database... ===
if ! /usr/sbin/sks cleandb; then fail; fi
echo === Building ptree database... ===
if ! /usr/sbin/sks pbuild -cache 20 -ptree_cache 70; then fail; fi
echo === Done! ===
