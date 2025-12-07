cmake_minimum_required(VERSION 3.25)

project(AdaCompiler LANGUAGES C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_EXTENSIONS OFF)

option(ENABLE_ASAN "Enable Address Sanitizer" OFF)
option(ENABLE_UBSAN "Enable Undefined Behavior Sanitizer" OFF)
option(ENABLE_GCC_ANALYZER "Enable GCC static analyzer" OFF)

add_custom_target(gen_keyword_hash
    COMMAND
        gperf --ignore-case --compare-strncmp --readonly-tables --language='ANSI-C' --struct-type
        --hash-function-name="hash_keyword" --lookup-function-name="is_keyword"
        --output-file="${CMAKE_SOURCE_DIR}/keywords.c"
        ${CMAKE_SOURCE_DIR}/keywords.gperf)

add_library(util STATIC src/util/error.c src/util/file_buffer.c src/util/string_view.c)
target_include_directories(util PUBLIC src/util)
target_compile_options(util PRIVATE "-Wall" "-Wextra" "-pedantic-errors")

add_executable(lexer_gen
    src/lexer_gen/main.c
    src/lexer_gen/lexer.c src/lexer_gen/parser.c src/lexer_gen/checker.c
    src/lexer_gen/debug.c)
target_include_directories(lexer_gen PRIVATE src/lexer_gen)
target_compile_options(lexer_gen PRIVATE "-Wall" "-Wextra" "-pedantic-errors")
target_link_libraries(lexer_gen PUBLIC util)

add_library(mini-gmp OBJECT third_party/mini-gmp/mini-gmp.c)
target_include_directories(mini-gmp PUBLIC third_party/mini-gmp)

add_executable(adac src/main.c src/token.c src/lexer.c src/parser.c src/debug.c src/string_pool.c)
target_include_directories(adac PRIVATE src)
target_compile_options(adac PRIVATE "-Wall" "-Wextra" "-pedantic-errors")
target_link_libraries(adac PRIVATE mini-gmp util)
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

add_custom_target(gen_lexer
    COMMAND python3 ${CMAKE_SOURCE_DIR}/scripts/gen_lexer_table.py > ${CMAKE_SOURCE_DIR}/src/lexer_table.c)

add_executable(string_pool_test tests/string_pool_test.c src/string_pool.c)
target_include_directories(string_pool_test PUBLIC src)
target_compile_options(string_pool_test PRIVATE "-Wall" "-Wextra" "-pedantic-errors")
target_link_libraries(string_pool_test PUBLIC util)
