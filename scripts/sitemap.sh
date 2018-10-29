#!/bin/bash

set -e
cd sitemap
elm make Sitemap.elm
elm reactor
