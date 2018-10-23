#!/bin/bash

set -e
cd tests
elm make Tests.elm --output=tests.js
elm reactor
