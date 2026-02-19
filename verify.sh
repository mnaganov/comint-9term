#!/bin/sh
rm -f out/*
ERRORS_FILE=out/elisp-errors.txt

echo "ansi-seq" > out/current-script
timeout 10s emacs -nw -q -l test/emacs.el
EMACS_STATUS=$?
if [ -f "$ERRORS_FILE" ]; then
    echo "Elisp errors:"
    cat "$ERRORS_FILE"
    if [ $EMACS_STATUS -eq 124 ]; then
        echo "Process timed out! The Elisp code must cause an infinite loop"
    fi
    echo "FAILURE"
    exit 2
fi
echo "apt-prog" > out/current-script
timeout 10s emacs -nw -q -l test/emacs.el
EMACS_STATUS=$?
if [ -f "$ERRORS_FILE" ]; then
    echo "Elisp errors:"
    cat "$ERRORS_FILE"
    if [ $EMACS_STATUS -eq 124 ]; then
        echo "Process timed out! The Elisp code must cause an infinite loop"
    fi
    echo "FAILURE"
    exit 2
fi

if [ ! -f "out/ansi-seq-out-shell.txt" ] || [ ! -f "out/ansi-seq-out-compile.txt" ] || [ ! -f "out/apt-prog-out-shell.txt" ] || [ ! -f "out/apt-prog-out-compile.txt" ]; then
    echo "Out is missing complete set of files. Check the Elisp code for errors."
    echo "FAILURE"
    exit 2
fi
export PAGER=cat
git diff --no-index test/ansi-seq.txt out/ansi-seq-out-shell.txt
STATUS1=$?
git diff --no-index test/ansi-seq.txt out/ansi-seq-out-compile.txt
STATUS2=$?

STTY_CONF_SHELL=`sed -nE 's/.*LINES=([0-9]+):COLUMNS=([0-9]+).*/rows \1 cols \2/p' out/apt-prog-out-shell.txt`
STTY_CONF_COMPILE=`sed -nE 's/.*LINES=([0-9]+):COLUMNS=([0-9]+).*/rows \1 cols \2/p' out/apt-prog-out-compile.txt`
screen -c test/screenrc -d -m -S my_session bash -c "stty ${STTY_CONF_SHELL}; screen -c test/screenrc -X scrollback 10000; test/apt-prog.sh; sleep 3; screen -X hardcopy -h out/apt-prog-out-shell-golden.txt"
screen -c test/screenrc -d -m -S my_session bash -c "stty ${STTY_CONF_COMPILE}; screen -c test/screenrc -X scrollback 10000; test/apt-prog.sh; sleep 3; screen -X hardcopy -h out/apt-prog-out-compile-golden.txt"
until [ -f out/apt-prog-out-shell-golden.txt ] && [ -f out/apt-prog-out-compile-golden.txt ]; do sleep 1; done
git diff --no-index out/apt-prog-out-shell-golden.txt out/apt-prog-out-shell.txt
STATUS3=$?
git diff --no-index out/apt-prog-out-compile-golden.txt out/apt-prog-out-compile.txt
STATUS4=$?
if [ "$STATUS1" -eq 0 ] && [ "$STATUS2" -eq 0 ] && [ "$STATUS3" -eq 0 ] && [ "$STATUS4" -eq 0 ]; then
    echo "SUCCESS"
    exit 0
else
    echo "FAILURE"
    exit 1
fi
