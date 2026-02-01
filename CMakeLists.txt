cmake_minimum_required(VERSION 3.25)

project(AdaCompiler LANGUAGES C)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_EXTENSIONS OFF)

set(FLAGS "-Wall" "-Wextra")
if(NOT (CMAKE_C_COMPILER_ID STREQUAL "TinyCC"))
    # TinyCC doesn't support this flag
    list(APPEND FLAGS "-pedantic-errors")
endif()

option(ENABLE_ASAN "Enable Address Sanitizer" OFF)
option(ENABLE_UBSAN "Enable Undefined Behavior Sanitizer" OFF)
option(ENABLE_GCC_ANALYZER "Enable GCC static analyzer" OFF)
option(ENABLE_CLANG_ANALYZER "Enable Clang static analyzer" OFF)

# Misc utilities
add_library(util STATIC src/util/error.c src/util/file_buffer.c src/util/string_view.c)
target_include_directories(util PUBLIC src/util)
target_compile_options(util PRIVATE "${FLAGS}")

# Arbitrary precision arithmetic library
add_library(mini-gmp OBJECT third_party/mini-gmp/mini-gmp.c)
target_include_directories(mini-gmp PUBLIC third_party/mini-gmp)

# Perfect hash function for recognizing Ada keywords
find_package(Gperf)
if(Gperf_FOUND)
    add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/src/keywords.c
        COMMAND "${Gperf_EXECUTABLE}" --ignore-case --compare-strncmp --readonly-tables --language='ANSI-C' --struct-type
            --hash-function-name="hash_keyword" --lookup-function-name="is_keyword"
            --output-file="keywords.c" keywords.gperf
        DEPENDS src/keywords.gperf
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src)
else()
    message(STATUS "Gperf not found - using the checked-in Gperf-generated files instead")
endif()

# Ada 83 lexer
find_package(FLEX)
if(FLEX_FOUND)
    add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/src/lex.yy.c ${CMAKE_CURRENT_SOURCE_DIR}/src/lexer.h
                       COMMAND "${FLEX_EXECUTABLE}" --header-file=lexer.h lexer83.l
                       DEPENDS src/lexer83.l src/keywords.c
                       WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src)
else()
    message(STATUS "Flex not found - using the checked-in Flex-generated files instead")
endif()

# Ada 83 parser
find_package(BISON)
if(BISON_FOUND)
    add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/src/grammar83.tab.c ${CMAKE_CURRENT_SOURCE_DIR}/src/parser.h
                       COMMAND "${BISON_EXECUTABLE}" --header=parser.h --warnings=deprecated grammar83.y
                       DEPENDS src/lex.yy.c src/grammar83.y
                       WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src)
else()
    message(STATUS "Bison not found - using the checked-in Bison-generated files instead")
endif()

# Compiler driver
add_executable(adac src/main.c src/grammar83.tab.c src/lex.yy.c src/string_pool.c)
target_include_directories(adac PRIVATE src)
target_compile_options(adac PRIVATE "${FLAGS}")
target_link_libraries(adac PUBLIC util mini-gmp)

if(ENABLE_ASAN)
    target_compile_options(adac PRIVATE "-fsanitize=address")
    target_link_options(adac PRIVATE "-fsanitize=address")
endif()
if(ENABLE_UBSAN)
    target_compile_options(adac PRIVATE "-fsanitize=undefined")
    target_link_options(adac PRIVATE "-fsanitize=undefined")
endif()
if(ENABLE_GCC_ANALYZER)
    # Flags:
    #  -Wno-analyzer-possible-null-dereference: Lots of false positives since we (purposefully) do not check calloc/malloc result
    target_compile_options(adac PRIVATE "-fanalyzer" "-Wno-analyzer-possible-null-dereference" "--param" "analyzer-max-svalue-depth=10000")
endif()
if(ENABLE_CLANG_ANALYZER)
    target_compile_options(util PRIVATE "--analyze")
    target_compile_options(adac PRIVATE "--analyze")
endif()

add_executable(string_pool_test tests/string_pool_test.c src/string_pool.c)
target_include_directories(string_pool_test PUBLIC src)
target_compile_options(string_pool_test PRIVATE "${FLAGS}")
target_link_libraries(string_pool_test PUBLIC util)

# Counts lines of code (requires cloc tool to be installed)
add_custom_target(cloc cloc --by-file --fullpath --not-match-d=src/lexer_gen --exclude-ext=tab.c,yy.c src
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

# Custom lexer generator (not current being used)
add_executable(lexer_gen
    src/lexer_gen/main.c
    src/lexer_gen/lexer.c src/lexer_gen/parser.c src/lexer_gen/checker.c src/lexer_gen/generator.c
    src/lexer_gen/debug.c)
target_include_directories(lexer_gen PRIVATE src/lexer_gen)
target_compile_options(lexer_gen PRIVATE "${FLAGS}")
target_link_libraries(lexer_gen PUBLIC util)
