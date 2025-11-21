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

add_library(mini-gmp OBJECT third_party/mini-gmp/mini-gmp.c)
target_include_directories(mini-gmp PUBLIC third_party/mini-gmp)

add_executable(adac src/main.c src/token.c src/lexer.c src/parser.c src/error.c src/debug.c src/string_pool.c)
target_include_directories(adac PRIVATE .)
target_compile_options(adac PRIVATE "-Wall" "-Wextra" "-pedantic-errors")
target_link_libraries(adac PRIVATE mini-gmp)

add_custom_target(gen_lexer
    COMMAND python3 ${CMAKE_SOURCE_DIR}/scripts/gen_lexer_table.py > ${CMAKE_SOURCE_DIR}/src/lexer_table.c)

add_executable(string_pool_test tests/string_pool_test.c src/string_pool.c)
target_include_directories(string_pool_test PUBLIC src)
target_compile_options(string_pool_test PRIVATE "-Wall" "-Wextra" "-pedantic-errors")
