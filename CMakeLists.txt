cmake_minimum_required(VERSION 3.25)

project(AdaCompiler LANGUAGES C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_EXTENSIONS OFF)

add_custom_target(gen_keyword_hash
    COMMAND
        gperf --ignore-case --compare-strncmp --readonly-tables --language='ANSI-C' --struct-type
        --hash-function-name="hash_keyword" --lookup-function-name="is_keyword"
        --output-file="${CMAKE_SOURCE_DIR}/keywords.c"
        ${CMAKE_SOURCE_DIR}/keywords.gperf)

add_executable(adac main.c lexer.c)
target_include_directories(adac PRIVATE .)
target_compile_options(adac PRIVATE "-Wall" "-Wextra" "-pedantic-errors")
