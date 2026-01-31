#!/usr/bin/env sh

set -e

find third_party/gnat-walnut-creek-Mar-94/ver_167/sparc/gnat-1.67-src/src/ -name *.ad[sb] -print | xargs -n 1 build/adac2
