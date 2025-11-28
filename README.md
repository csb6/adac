# Ada compiler

My attempt to make a compiler for enough of the Ada '83 programming language to compile an early version
of the GNAT Ada compiler, enabling GNAT to be bootstrapped.

GNAT is written mostly in Ada. This causes a chicken-and-egg problem: an Ada compiler is required to build it. This means that a prebuilt binary of GNAT (built
using earlier versions of GNAT, themselves built using a proprietary Ada compiler
during GNAT's early development) is necessary to build GNAT. This project aims to avoid the need for this potentially malicious binary in the bootstrap process.

## Progress

- The lexer is largely complete, although it does not currently support:
  - Based literals with points
  - Substitution of '#' with '"' within based literals

- The parser is in the early stages. It currently supports:
  - Integer, enumeration, subtype, and derived type definitions (although no constraints are supported besides ranges on integer types)
  - Object (and constant) declarations
  - Subprogram declarations and bodies
  - Null, assignment, procedure call, return, block, if, case, exit, loop, and goto statements
  - Binary/unary expressions
  - Integer, string, and character literals
  - Some checking for redefinitions of names within same region
  - Package specifications containing the supported kinds of declarations

No semantic analysis or code generation is currently implemented.

## Building

The build system is CMake:

    mkdir build
    cd build
    cmake ..
    # Here we use ninja, but use whichever CMake generator you prefer
    ninja

However, if you don't want to use CMake, it is also possible to build this project using a manual compiler
invocation. Reference CMakeLists.txt to see the source files needed. It should be as simple as passing
all of the .c files (except `keywords.c` and `lexer_table.c`) into the compiler driver.

Note: [`gperf`](https://www.gnu.org/software/gperf/) is not a compile-time or runtime dependency because the output of gperf (`keywords.c`) is checked into
the source tree. To regenerate `keywords.c`, you can run the `gen_keyword_hash` custom command (e.g. `ninja gen_keyword_hash`)
or manually invoke `gperf` using the invocation in the comment at the top of `keywords.c`.

## Running

The build produces a standalone executable `adac` that can then be run:

    adac path_to_ada_source_file

Currently, this simply parses the given file, emitting error messages if any are encountered, and pretty prints the parse tree.

## Useful links

- [Bookstrapping wiki article](https://bootstrapping.miraheze.org/wiki/Bootstrapping_Specific_Languages#Ada_and_SPARK) describing the issues with bootstrapping GNAT
- [Ada '83 Language Reference Manual](http://archive.adaic.com/standards/83lrm/html/ada_lrm.html)

## License

Files in the `third_party` directory are vendored and have their own separate copyrights/licenses. See the respective README
files of each subdirectory for more information.

All other files in this repository (unless otherwise specified) are licensed under the [GNU General Public License, version 3](https://www.gnu.org/licenses/gpl-3.0.html).
