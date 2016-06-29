#!/usr/bin/bash

$(dirname $0)/find-elm-files.sh | xargs egrep -Hn --color=always $1
