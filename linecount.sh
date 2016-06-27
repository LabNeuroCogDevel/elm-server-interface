#!/usr/bin/bash

cd "$(dirname "$0")"

find src/ -iname "*.elm" -not -ipath "*elm-stuff*" | xargs cat | egrep "^.+$" | egrep -v "(^\s*--)|(^\s*[][()}{]*\s*$)" | wc
