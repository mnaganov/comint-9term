#!/bin/sh
: ${APT_MESSAGES:=3000}
: ${LINES:=$(tput lines)}
echo "9TERM_SET_HEIGHT=${LINES}"
exec "$(dirname "$0")/apt-prog-impl.sh" --messages "$APT_MESSAGES"
