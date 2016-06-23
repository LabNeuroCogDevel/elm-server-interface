#!/usr/bin/bash

mkdir -p build

elm make src/Main.elm --output build/main.js

