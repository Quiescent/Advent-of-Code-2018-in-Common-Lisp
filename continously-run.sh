#!/bin/bash

if [ -z "$1" ]; then
    echo "Please supply the day to continuously run..."
    exit 1
fi

ORIGINAL_DIR=$(pwd)

if [ "$(uname)" == "Darwin" ]; then
    while [ 1 == 1 ]; do
        echo "======================================"
        echo "Restarting watch loop due to errors..."
        echo "======================================"
        sbcl --script $1
        echo "fswatch -0 \"$(pwd)/$1\" | xargs -0 -n1 -I '{}' sbcl --script \"{}\""
        fswatch -0 "$(pwd)/$1" | xargs -0 -n1 -I '{}' sbcl --script "{}"
    done
else
    inotifywait -e close_write,moved_to,create -m "lisp/$1" |
        while read -r directory events filename; do
            echo "cd lisp && sbcl --script $1"
            (cd lisp && sbcl --script "$1") || cd "$ORIGINAL_DIR"
done
fi
