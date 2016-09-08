#!/usr/bin/env bash

cd "$(dirname "$0")"

mkdir -p build

elm make src/Main.elm --output build/main.js

