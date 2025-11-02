#ifndef ADA_PARSER_H
#define ADA_PARSER_H

struct PackageSpec_;

struct PackageSpec_* parser_parse(const char* input_start, const char* input_end);

void print_package_spec(const struct PackageSpec_* package_spec);

#endif /* ADA_PARSER_H */
