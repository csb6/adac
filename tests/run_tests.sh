#!/usr/bin/env sh

set -e

find third_party/83acvc/ -name *.ada -print | xargs -n 1 build/adac
