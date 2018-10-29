#!/bin/bash

set -e
cd sitemap
elm make Sitemap.elm --output=sitemap.js
elm reactor
