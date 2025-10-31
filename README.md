# Ada compiler

My attempt to make a compiler for enough of the Ada '83 programming language to compile an early version
of the GNAT Ada compiler into C, enabling GNAT to be bootstrapped on any platform with a C compiler.

GNAT is part of GCC and is written in Ada, which causes bootstrapping issues
when compiling GCC from source code. It means that a prebuilt binary of GNAT (built
using earlier versions of GNAT, themselves built using a proprietary Ada compiler
during GNAT's early development before GNAT became self-hosting) is necessary to build
GNAT from source.

## Progress

- The lexer is largely complete, although it is lacking some validation checks.
- The parser is in the early stages.

## Building

    mkdir build
    cd build
    cmake ..
    # Here we use ninja, but use whichever CMake generator you prefer
    ninja

Since CMake itself requires a C++ compiler to bootstrap, it is also possible to build using a manual compiler
invocation. Reference the CMakeLists.txt files to see the source files needed. It should be as simple as passing
all of the .c files (except `keywords.c`) into the compiler driver.

Note: [`gperf`](https://www.gnu.org/software/gperf/) is not a compile-time or runtime dependency because the output of gperf (`keywords.c`) is checked into
the source tree. To regenerate `keywords.c`, you can run the `gen_keyword_hash` custom command (e.g. `ninja gen_keyword_hash`)
or manually invoke `gperf` using the invocation in the comment at the top of `keywords.c`.

## Running

The build produces a standalone executable `adac` that can then be run:

    adac path_to_ada_source_file

Currently, this simply tokenizes to given file and prints out information about each token.

## Useful links

- [Bookstrapping wiki article](https://bootstrapping.miraheze.org/wiki/Bootstrapping_Specific_Languages#Ada_and_SPARK) describing the issues with bootstrapping GNAT
- [Ada '83 Language Reference Manual](http://archive.adaic.com/standards/83lrm/html/ada_lrm.html)

## License

Files in the `third_party` directory are vendored and have their own separate copyrights/licenses. See the respective README
files of each subdirectory for more information.

All other files in this repository (unless otherwise specified) are licensed under the [GNU General Public License, version 3](https://www.gnu.org/licenses/gpl-3.0.html).
