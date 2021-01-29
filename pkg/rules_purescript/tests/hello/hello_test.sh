#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

program="$1"
got=$("$program")
want="Hello, world!"

if [ "$got" != "$want" ]; then
    cat >$2 <<EOF
got:
$got

want:
$want
EOF
    exit 1
fi
