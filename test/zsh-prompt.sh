#!/bin/bash

# ==============================================================================
# Use Cases Covered:
# Simulates a Zsh prompt containing advanced control sequences like bracketed
# paste mode (`\e[?2004h`/`l`) and heavy use of carriage returns (`\r`) with
# literal space padding. It specifically targets macOS `terminfo` quirks where
# `TERM=vt50` pads lines with spaces due to missing line-erase capabilities.
# ==============================================================================
echo "=== zsh-prompt Test Suite ==="
echo "SCREEN_DIMS: LINES=24:COLUMNS=126"
echo "9TERM_SET_HEIGHT=24"

# Line 1: Space, Backspace, then 95 spaces followed by bracketed paste disable and CR/LF
printf " \010                                                                                               \033[?2004l\015\015\012"
sleep 0.1

# Line 2: Percent sign followed by 123 spaces and CR-Space-CR
printf "%%                                                                                                                           \015 \015"
sleep 0.1

# Line 3: CR, Erase Display, prompt text, Erase Line, then bracketed paste enable
printf "\015\033Jmikha@Mikhails-Laptop ~ %% \033K\033[?2004h"
sleep 0.1

# Line 4: Typing 'echo aaa', padding, Erase Line, CR/LF, then cleanup sequences
printf "e\010echo aaa                                                                                           \033K\015\015\012\033K \015   \033[?2004l\015\015\012"
sleep 0.1

# Line 5: Output of 'aaa', then prompt padding again
printf "aaa\012%%                                                                                                                          \015 \015"
sleep 0.1

# Line 6: Restore prompt
printf "\015\033Jmikha@Mikhails-Laptop ~ %% \033K\033[?2004h"
sleep 0.1

echo ""
echo "=== Test Suite Complete ==="
