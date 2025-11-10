# TODO: write a proper lexer generator so no need for Python, less ad-hoc
from itertools import chain
import sys

# Taken from token.h (not including keywords)
tokens = [
    # Final states (i.e. token kinds)
    #  Single character elements
    "TOKEN_AMP", "TOKEN_L_PAREN", "TOKEN_R_PAREN", "TOKEN_PLUS", "TOKEN_COMMA", "TOKEN_SEMICOLON", "TOKEN_BAR",
    "TOKEN_SINGLE_QUOTE", "TOKEN_MULT", "TOKEN_MINUS", "TOKEN_DOT", "TOKEN_DIVIDE", "TOKEN_COLON",
    "TOKEN_LT", "TOKEN_EQ", "TOKEN_GT",
    #  Multi-character elements
    "TOKEN_NEQ", "TOKEN_GTE", "TOKEN_LTE", "TOKEN_ARROW", "TOKEN_DOUBLE_DOT", "TOKEN_EXP", "TOKEN_ASSIGN", "TOKEN_L_LABEL_BRACKET",
    "TOKEN_R_LABEL_BRACKET", "TOKEN_BOX",
    #  Identifier
    "TOKEN_IDENT",
    #  Literals
    "TOKEN_NUM_LITERAL", "TOKEN_CHAR_LITERAL", "TOKEN_STRING_LITERAL",
    "TOKEN_ERROR"
]

classes = [
    "CLASS_OTHER", "CLASS_DIGIT", "CLASS_LETTER", "CLASS_E", "CLASS_WS", "CLASS_NEWLINE", "CLASS_AMP", "CLASS_S_QUOTE", "CLASS_D_QUOTE",
    "CLASS_L_PAREN", "CLASS_R_PAREN", "CLASS_TIMES", "CLASS_PLUS", "CLASS_COMMA", "CLASS_MINUS", "CLASS_DOT",
    "CLASS_DIVIDE", "CLASS_COLON", "CLASS_SEMICOLON", "CLASS_LT", "CLASS_EQ", "CLASS_GT", "CLASS_BAR", "CLASS_USCORE",
    "CLASS_HASH"
]

skip_table = {
    "STATE_START": {
        "CLASS_WS": "STATE_START",
        "CLASS_NEWLINE": "STATE_START",
        "CLASS_MINUS": "STATE_MINUS_COMMENT",
        "others": "TOKEN_EOF"
    },
    "STATE_MINUS_COMMENT": {
        "CLASS_MINUS": "STATE_COMMENT",
        "others": "TOKEN_MINUS"
    },
    "STATE_COMMENT": {
        "CLASS_NEWLINE": "STATE_START",
        "others": "STATE_COMMENT"
    }
}

state_table = {
    "STATE_START": {
        # Single character tokens
        "CLASS_AMP": "TOKEN_AMP",
        "CLASS_L_PAREN": "TOKEN_L_PAREN",
        "CLASS_R_PAREN": "TOKEN_R_PAREN",
        "CLASS_PLUS": "TOKEN_PLUS",
        "CLASS_COMMA": "TOKEN_COMMA",
        "CLASS_SEMICOLON": "TOKEN_SEMICOLON",
        "CLASS_BAR": "TOKEN_BAR",
        "CLASS_MINUS": "TOKEN_MINUS",
        # Fixed-length, multi character tokens
        "CLASS_TIMES": "STATE_TIMES_EXP",
        "CLASS_DOT": "STATE_DOT_DOUBLE_DOT",
        "CLASS_DIVIDE": "STATE_DIVIDE_NEQ",
        "CLASS_COLON": "STATE_COLON_ASSIGN",
        "CLASS_LT": "STATE_LT_LTE_LB_BOX",
        "CLASS_EQ": "STATE_EQ_ARROW",
        "CLASS_GT": "STATE_GT_GTE_GB",
        # Complex tokens
        "CLASS_LETTER": "STATE_IN_IDENTIFIER",
        "CLASS_E": "STATE_IN_IDENTIFIER",
        "CLASS_D_QUOTE": "STATE_IN_STRING_LITERAL",
        "CLASS_S_QUOTE": "STATE_IN_CHAR_LITERAL_1",
        "CLASS_DIGIT": "STATE_IN_NUM_LIT",
        # Else
        "others": "TOKEN_ERROR"
    },
    "STATE_TIMES_EXP": {
        "CLASS_TIMES": "TOKEN_EXP",
        "others": "TOKEN_MULT"
    },
    "STATE_DOT_DOUBLE_DOT": {
        "CLASS_DOT": "TOKEN_DOUBLE_DOT",
        "others": "TOKEN_DOT"
    },
    "STATE_DIVIDE_NEQ": {
        "CLASS_EQ": "TOKEN_NEQ",
        "others": "TOKEN_DIVIDE"
    },
    "STATE_COLON_ASSIGN": {
        "CLASS_EQ": "TOKEN_ASSIGN",
        "others": "TOKEN_COLON"
    },
    "STATE_LT_LTE_LB_BOX": {
        "CLASS_EQ": "TOKEN_LTE",
        "CLASS_LT": "TOKEN_L_LABEL_BRACKET",
        "CLASS_GT": "TOKEN_BOX",
        "others": "TOKEN_LT"
    },
    "STATE_EQ_ARROW": {
        "CLASS_GT": "TOKEN_ARROW",
        "others": "TOKEN_EQ"
    },
    "STATE_GT_GTE_GB": {
        "CLASS_EQ": "TOKEN_GTE",
        "CLASS_LT": "TOKEN_R_LABEL_BRACKET",
        "others": "TOKEN_GT"
    },
    "STATE_IN_IDENTIFIER": {
        "CLASS_LETTER": "STATE_IN_IDENTIFIER",
        "CLASS_E": "STATE_IN_IDENTIFIER",
        "CLASS_DIGIT": "STATE_IN_IDENTIFIER",
        "CLASS_USCORE": "STATE_IN_IDENT_USCORE",
        "others": "TOKEN_IDENT" # Hand-written code can switch to specific keyword if needed
    },
    # Ensures only 1 underscore in a row
    "STATE_IN_IDENT_USCORE": {
        "CLASS_LETTER": "STATE_IN_IDENTIFIER",
        "CLASS_E": "STATE_IN_IDENTIFIER",
        "CLASS_DIGIT": "STATE_IN_IDENTIFIER",
        "CLASS_USCORE": "TOKEN_ERROR",
        "others": "TOKEN_IDENT" # Hand-written code can switch to specific keyword if needed
    },
    "STATE_IN_CHAR_LITERAL_1": {
        # TODO: restrict to graphic_characters
        "others": "STATE_IN_CHAR_LITERAL_2"
    },
    "STATE_IN_CHAR_LITERAL_2": {
        "CLASS_S_QUOTE": "TOKEN_CHAR_LITERAL",
        "others": "TOKEN_SINGLE_QUOTE" # Special case: set token length to 1
    },
    "STATE_IN_STRING_LITERAL": {
        # TODO: restrict to graphic_characters
        "CLASS_D_QUOTE": "STATE_IN_D_QUOTE",
        "others": "STATE_IN_STRING_LITERAL"
    },
    "STATE_IN_D_QUOTE": {
        "CLASS_D_QUOTE": "STATE_IN_STRING_LITERAL", # Escapes '"'
        "others": "TOKEN_STRING_LITERAL"
    },
    "STATE_IN_NUM_LIT": {
        "CLASS_DIGIT": "STATE_IN_NUM_LIT",
        "CLASS_USCORE": "STATE_IN_NUM_USCORE",
        "CLASS_HASH": "STATE_IN_BASED_1",
        "CLASS_DOT": "STATE_IN_DECIMAL_1",
        "CLASS_E": "STATE_IN_EXP_1",
        "others": "TOKEN_NUM_LITERAL"
    },
    "STATE_IN_NUM_USCORE": {
        "CLASS_DIGIT": "STATE_IN_NUM_LIT", # Only one underscore in a row permitted
        "others": "TOKEN_ERROR"
    },
    "STATE_IN_BASED_1": {
        "CLASS_DIGIT": "STATE_IN_BASED_1",
        "CLASS_LETTER": "STATE_IN_BASED_1",
        "CLASS_E": "STATE_IN_BASED_1",
        "CLASS_USCORE": "STATE_IN_BASED_USCORE",
        # "CLASS_DOT" TODO: support based decimal literals
        "CLASS_HASH": "STATE_IN_BASED_2",
        "others": "TOKEN_ERROR"
    },
    "STATE_IN_BASED_USCORE": {
        "CLASS_DIGIT": "STATE_IN_BASED_1", # Only one underscore in a row permitted
        "CLASS_LETTER": "STATE_IN_BASED_1",
        "CLASS_E": "STATE_IN_BASED_1",
        "others": "TOKEN_ERROR"
    },
    "STATE_IN_BASED_2": {
        "CLASS_E": "STATE_IN_EXP_1",
        "others": "TOKEN_NUM_LITERAL"
    },
    "STATE_IN_DECIMAL_1": {
        "CLASS_DIGIT": "STATE_IN_DECIMAL_2", # Require at least 1 digit after '.'
        "others": "TOKEN_ERROR"
    },
    "STATE_IN_DECIMAL_2": {
        "CLASS_DIGIT": "STATE_IN_DECIMAL_2",
        "CLASS_USCORE": "STATE_IN_DEC_USCORE",
        "CLASS_E": "STATE_IN_EXP_1",
        "others": "TOKEN_NUM_LITERAL"
    },
    # Ensures only one underscore in a row permitted
    "STATE_IN_DEC_USCORE": {
        "CLASS_DIGIT": "STATE_IN_DECIMAL_2",
        "others": "TOKEN_ERROR"
    },
    "STATE_IN_EXP_1": {
        "CLASS_PLUS": "STATE_IN_EXP_2",
        "CLASS_MINUS": "STATE_IN_EXP_2",
        "CLASS_DIGIT": "STATE_IN_EXP_3",
        # CLASS_USCORE TODO: support underscores in exponents
        "others": "TOKEN_ERROR"
    },
    # Ensures at least 1 digit is after '+'/'-'
    "STATE_IN_EXP_2": {
        "CLASS_DIGIT": "STATE_IN_EXP_3",
        "others": "TOKEN_ERROR"
    },
    "STATE_IN_EXP_3": {
        "CLASS_DIGIT": "STATE_IN_EXP_3",
        "others": "TOKEN_NUM_LITERAL"
    }
}

move_table = {
    "TOKEN_AMP": 1,
    "TOKEN_L_PAREN": 1,
    "TOKEN_R_PAREN": 1,
    "TOKEN_PLUS": 1,
    "TOKEN_COMMA": 1,
    "TOKEN_SEMICOLON": 1,
    "TOKEN_MINUS": 1,
    "TOKEN_BAR": 1,

    "TOKEN_EXP": 1,
    "TOKEN_MULT": 0,
    "TOKEN_DOUBLE_DOT": 1,
    "TOKEN_DOT": 0,
    "TOKEN_NEQ": 1,
    "TOKEN_DIVIDE": 0,
    "TOKEN_ASSIGN": 1,
    "TOKEN_COLON": 0,
    "TOKEN_LTE": 1,
    "TOKEN_L_LABEL_BRACKET": 1,
    "TOKEN_BOX": 1,
    "TOKEN_LT": 0,
    "TOKEN_ARROW": 1,
    "TOKEN_EQ": 0,
    "TOKEN_GTE": 1,
    "TOKEN_R_LABEL_BRACKET": 1,
    "TOKEN_GT": 0,
    "TOKEN_IDENT": 0,
    "TOKEN_CHAR_LITERAL": 1,
    "TOKEN_SINGLE_QUOTE": 0,
    "TOKEN_STRING_LITERAL": 0,
    "TOKEN_NUM_LITERAL": 0,
    "TOKEN_ERROR": 0,

    # Every 'STATE_' state will advance by 1 (since these are intermediate states)
    "others": 1
}

def print_enum_types():
    print("typedef uint8_t Class;")
    print("enum {")
    for c in classes:
        print(f"  {c},")
    print(f"\n  CLASS_COUNT")
    print("};\n")

    print("typedef uint8_t State;")
    print("enum {")
    states = list(state_table.keys())
    print(f"  {states[0]} = TOKEN_NUM_TOKEN_KINDS,")
    for s in states[1:]:
        print(f"  {s},")
    print(f"\n  STATE_COUNT = {len(state_table)}")
    print("};\n")

    print("enum {")
    print("  STATE_MINUS_COMMENT = STATE_START + 1,")
    print("  STATE_COMMENT")
    print("};\n")

def print_char_entry(c, class_):
    print(f"  ['{c}'] = {class_},")

def print_class_table():
    print("// Default class (class 0) is CLASS_OTHER")
    print("static const Class class_table[256] = {")
    # Digits
    for c in range(ord('0'), ord('9')+1):
        print_char_entry(chr(c), "CLASS_DIGIT")
    # Letters
    for c in chain(range(ord('a'), ord('e')), range(ord('f'), ord('z')+1)):
        print_char_entry(chr(c), "CLASS_LETTER")
    for c in chain(range(ord('A'), ord('E')), range(ord('F'), ord('Z')+1)):
        print_char_entry(chr(c), "CLASS_LETTER")
    print_char_entry('E', "CLASS_E")
    print_char_entry('e', "CLASS_E")
    # Whitespace
    print_char_entry("\\n", "CLASS_NEWLINE")
    for c in [" ", "\\f", "\\r", "\\t", "\\v"]:
        print_char_entry(c, "CLASS_WS")
    # Classes of their own
    class_chars = [
        ("CLASS_AMP", '&'),
        ("CLASS_S_QUOTE", "\\'"),
        ("CLASS_D_QUOTE", '"'),
        ("CLASS_L_PAREN", '('),
        ("CLASS_R_PAREN", ')'),
        ("CLASS_TIMES", '*'),
        ("CLASS_PLUS", '+'),
        ("CLASS_COMMA", ','),
        ("CLASS_MINUS", '-'),
        ("CLASS_DOT", '.'),
        ("CLASS_DIVIDE", '/'),
        ("CLASS_COLON", ':'),
        ("CLASS_SEMICOLON", ';'),
        ("CLASS_LT", '<'),
        ("CLASS_EQ", '='),
        ("CLASS_GT", '>'),
        ("CLASS_BAR", '|'),
        ("CLASS_BAR", '!'),
        ("CLASS_USCORE", '_'),
        ("CLASS_HASH", '#')
    ]
    for class_, c in class_chars:
        print_char_entry(c, class_)
    print("};\n")

def validate_state_table(state_table, expected_tokens):
    found_error = False
    intermediate_states = set(state_table.keys())
    unreachable_tokens = set(expected_tokens)
    classes_set = set(classes)
    for state, transitions in state_table.items():
        succ_states = set(transitions.values())

        unhandled_states = {s for s in succ_states if not s.startswith("TOKEN_")} - intermediate_states
        if len(unhandled_states) > 0:
            print(f"Error: {unhandled_states} have no transitions in the state table", file=sys.stderr)
            found_error = True

        unknown_tokens = {t for t in succ_states if t.startswith("TOKEN_") and t not in expected_tokens}
        if len(unknown_tokens) > 0:
            print(f"Error: {unknown_tokens} are not valid tokens", file=sys.stderr)
            found_error = True

        state_classes = set(transitions.keys()) - {"others"}
        unknown_classes = state_classes - classes_set
        if len(unknown_classes) > 0:
            print(f"Error: {unknown_classes} are not valid classes", file=sys.stderr)
            found_error = True

        if "others" not in transitions.keys():
            print(f"Error: missing others for state {state}", file=sys.stderr)
            found_error = True
        unreachable_tokens -= succ_states
    if len(unreachable_tokens) > 0:
        print(f"Error: {unreachable_tokens} are not produced by the state machine", file=sys.stderr)
        found_error = True
    return found_error

def validate_move_table():
    found_error = False
    unset_tokens = set(tokens) - set(move_table.keys())
    if len(unset_tokens) > 0:
        print(f"Error: {unset_tokens} are not explicitly listed in move table", file=sys.stderr)
        found_error = True
    return found_error

def generate_c_move_table():
    print("static const uint8_t move_table[TOKEN_NUM_TOKEN_KINDS + STATE_COUNT] = {")
    for token in tokens:
        print(f"  [{token}] = {move_table[token]},")
    for state in state_table.keys():
        print(f"  [{state}] = {move_table["others"]},")
    print("};")

def generate_c_state_table(state_table, state_count, table_name):
    class_to_idx = {c: i for i, c in enumerate(classes)}
    print(f"static const State {table_name}[{state_count}][CLASS_COUNT] = {{")
    for state, transitions in state_table.items():
        row = {class_to_idx[c]: succ_state for c, succ_state in transitions.items() if c != "others"}
        print(f"  /*{state}*/ {{", end="")
        for i in range(len(classes)):
            succ_state = row.get(i, transitions["others"])
            print(f"{succ_state},", end="")
        print("},")
    print("};")
    print(f"Table size: {len(classes) * len(state_table)}", file=sys.stderr)

if(validate_state_table(skip_table, {"TOKEN_EOF", "TOKEN_MINUS"})):
    exit(1)
if(validate_state_table(state_table, tokens)):
    exit(1)
if(validate_move_table()):
    exit(1)

print("// Autogenerated from gen_lexer_table.py")
print("#include <stdint.h>")
print("#include \"token.h\"\n")
print_enum_types()
print_class_table()
generate_c_state_table(skip_table, len(skip_table), "skip_table")
print()
generate_c_state_table(state_table, "STATE_COUNT", "state_table")
print()
generate_c_move_table()
