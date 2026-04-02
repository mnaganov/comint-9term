#!/bin/sh
: ${APT_MESSAGES:=3000}
exec "$(dirname "$0")/apt-prog-impl.sh" --messages "$APT_MESSAGES"
