#!/bin/bash

mkdir chaldeas
mkdir chaldeas/js
set -e
elm make src/Main.elm --output=chaldeas/js/chaldeas.js
elm reactor
