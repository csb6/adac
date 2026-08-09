#include <stdio.h>
#include <stdbool.h>
#include "debug.h"
#include "error.h"
#include "parser.h"
#include "lexer.h"
#include "string_pool.h"

void yyerror(YYLTYPE* yyloc, yyscan_t scanner, ParseContext* parse_ctx, const char* msg)
{
    (void)scanner;
    (void)parse_ctx;
    error_print(*yyloc, msg);
    error_exit();
}

int main(int argc, const char** argv)
{
    if(argc != 2) {
        fprintf(stderr, "Usage: %s input_file\n", argv[0]);
        return 1;
    }
    const char* input_file_path = argv[1];
    FILE* input_file = fopen(input_file_path, "rb");
    if(!input_file) {
        perror(input_file_path);
        return 1;
    }
    yyscan_t lexer;
    error_set_source_file_path(input_file_path);
    yylex_init(&lexer);
    yyset_in(input_file, lexer);

    string_pool_init();
    ParseContext parse_ctx = {0};
    int parse_status = yyparse(lexer, &parse_ctx);
    yylex_destroy(lexer);
    fclose(input_file);
    if(parse_status != 0) {
        fprintf(stderr, "Compilation failed\n");
        return 1;
    }
    print_compilation_unit(parse_ctx.comp_unit);

    return 0;
}
