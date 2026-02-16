#!/bin/sh
rm -f out/*
emacs -nw -q -l test/emacs.el
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
