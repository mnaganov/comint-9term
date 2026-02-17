#!/bin/sh
rm -f out/*
timeout 2s emacs -nw -q -l test/emacs.el
EMACS_STATUS=$?
if [ $EMACS_STATUS -eq 124 ]; then
    echo "Process timed out! The Elisp code must cause an infinite loop"
    echo "FAILURE"
    exit 3
fi
if [ -z "$(ls -A "out/")" ]; then
    echo "Out is empty. Check the Elisp code for errors."
    echo "FAILURE"
    exit 2
fi
ERRORS_FILE=out/elisp-errors.txt
if [ -f "$ERRORS_FILE" ]; then
    echo "Elisp errors:"
    cat "$ERRORS_FILE"
    echo "FAILURE"
    exit 2
fi
export PAGER=cat
git diff --no-index test/ansi-seq.txt out/ansi-seq-out-shell.txt
STATUS1=$?
git diff --no-index test/ansi-seq.txt out/ansi-seq-out-compile.txt
STATUS2=$?
git diff --no-index test/apt-prog.txt out/apt-prog-out-shell.txt
STATUS3=$?
git diff --no-index test/apt-prog.txt out/apt-prog-out-compile.txt
STATUS4=$?
if [ "$STATUS1" -eq 0 ] && [ "$STATUS2" -eq 0 ] && [ "$STATUS3" -eq 0 ] && [ "$STATUS4" -eq 0 ]; then
    echo "SUCCESS"
    exit 0
else
    echo "FAILURE"
    exit 1
fi
