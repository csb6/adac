/*
Ada compiler
Copyright (C) 2025  Cole Blakley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include "error.h"
#include "parser.h"
#include "token.h"

static
void print_token(Token, const char* input);

static
void usage(const char* exe)
{
    fprintf(stderr, "Usage: %s source_file\n", exe);
}

static
off_t get_file_size(int fd)
{
    struct stat file_info = {0};
    if(fstat(fd, &file_info) < 0) {
        perror("Failed to read metadata of input file");
        return -1;
    }
    return file_info.st_size;
}

int main(int argc, char** argv)
{
    if(argc != 2) {
        usage(argv[0]);
        return 1;
    }

    const char* source_file_path = argv[1];
    error_set_source_file_path(source_file_path);
    int fd = open(source_file_path, O_RDONLY);
    if(fd < 0) {
        perror("Failed to open file");
        return 1;
    }
    off_t file_size = get_file_size(fd);
    if(file_size < 0) {
        perror("Failed to read metadata of input file");
        return 1;
    }

    void* input_file = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if(input_file == MAP_FAILED) {
        perror("Failed to read input file");
        return 1;
    }

    const char* input_start = input_file;
    const char* input_end = input_start + file_size;
    parser_parse(input_start, input_end);

    if(munmap(input_file, file_size) < 0) {
        perror("Failed to unmap input file");
        return 1;
    }
    close(fd);

    return error_get_return_status();
}

static const char* token_kind_names[TOKEN_NUM_TOKEN_KINDS] = {
    [TOKEN_AMP] = "AMP",
    [TOKEN_SINGLE_QUOTE] = "SINGLE_QUOTE",
    [TOKEN_L_PAREN] = "L_PAREN",
    [TOKEN_R_PAREN] = "R_PAREN",
    [TOKEN_MULT] = "MULT",
    [TOKEN_PLUS] = "PLUS",
    [TOKEN_COMMA] = "COMMA",
    [TOKEN_MINUS] = "MINUS",
    [TOKEN_DOT] = "DOT",
    [TOKEN_DIVIDE] = "DIVIDE",
    [TOKEN_COLON] = "COLON",
    [TOKEN_SEMICOLON] = "SEMICOLON",
    [TOKEN_LT] = "LT",
    [TOKEN_EQ] = "EQ",
    [TOKEN_GT] = "GT",
    [TOKEN_NEQ] = "NEQ",
    [TOKEN_GTE] = "GTE",
    [TOKEN_LTE] = "LTE",
    [TOKEN_BAR] = "BAR",
    [TOKEN_ARROW] = "ARROW",
    [TOKEN_DOUBLE_DOT] = "DOUBLE_DOT",
    [TOKEN_EXP] = "EXP",
    [TOKEN_ASSIGN] = "ASSIGN",
    [TOKEN_L_LABEL_BRACKET] = "L_LABEL_BRACKET",
    [TOKEN_R_LABEL_BRACKET] = "R_LABEL_BRACKET",
    [TOKEN_BOX] = "BOX",
    [TOKEN_EOF] = "EOF",
    [TOKEN_CHAR_LITERAL] = "CHAR_LITERAL",
    [TOKEN_IDENT] = "IDENT",
    [TOKEN_NUM_LITERAL] = "NUM_LITERAL",
    [TOKEN_PACKAGE] = "PACKAGE",
    [TOKEN_END] = "END",
    [TOKEN_STRING_LITERAL] = "STRING_LITERAL",
};

static
void print_token(Token token, const char* input)
{
    const char* token_name;
    if(token.kind > TOKEN_NUM_TOKEN_KINDS) {
        token_name = "Invalid token";
    } else {
        token_name = token_kind_names[token.kind];
        if(token_name == NULL) {
            token_name = "Unhandled token";
        }
    }
    printf("%s\n  Text: '%.*s'\n", token_name, (int)token.len, input + token.start);
}
