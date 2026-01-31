/* ANSI-C code produced by gperf version 3.2.1 */
/* Command-line: gperf --ignore-case --compare-strncmp --readonly-tables --language=ANSI-C --struct-type --hash-function-name=hash_keyword --lookup-function-name=is_keyword --output-file=keywords.c keywords.gperf  */
/* Computed positions: -k'1,3,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

#line 1 "keywords.gperf"

#include "parser.h"
#line 4 "keywords.gperf"
struct keyword_token { const char* name; enum yytokentype kind; };

#define TOTAL_KEYWORDS 63
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 9
#define MIN_HASH_VALUE 3
#define MAX_HASH_VALUE 120
/* maximum key range = 118, duplicates = 0 */

#ifndef GPERF_DOWNCASE
#define GPERF_DOWNCASE 1
static const unsigned char gperf_downcase[256] =
  {
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
     15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
     30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,
     45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,
     60,  61,  62,  63,  64,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106,
    107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
    122,  91,  92,  93,  94,  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
    105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
    120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134,
    135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
    150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164,
    165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
    180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194,
    195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
    210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224,
    225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
    240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254,
    255
  };
#endif

#ifndef GPERF_CASE_STRNCMP
#define GPERF_CASE_STRNCMP 1
static int
gperf_case_strncmp (register const char *s1, register const char *s2, register size_t n)
{
  for (; n > 0;)
    {
      unsigned char c1 = gperf_downcase[(unsigned char)*s1++];
      unsigned char c2 = gperf_downcase[(unsigned char)*s2++];
      if (c1 != 0 && c1 == c2)
        {
          n--;
          continue;
        }
      return (int)c1 - (int)c2;
    }
  return 0;
}
#endif

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash_keyword (register const char *str, register size_t len)
{
  static const unsigned char asso_values[] =
    {
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121,  20,  45,   5,  35,   0,
       13,  50,  55,  45, 121,  15,  20,  40,  20,   5,
       40, 121,   0,  10,   0,  10,   5,  15,   0,  10,
      121, 121, 121, 121, 121, 121, 121,  20,  45,   5,
       35,   0,  13,  50,  55,  45, 121,  15,  20,  40,
       20,   5,  40, 121,   0,  10,   0,  10,   5,  15,
        0,  10, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121, 121, 121, 121, 121,
      121, 121, 121, 121, 121, 121
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[2]];
#if (defined __cplusplus && (__cplusplus >= 201703L || (__cplusplus >= 201103L && defined __clang__ && __clang_major__ + (__clang_minor__ >= 9) > 3))) || (defined __STDC_VERSION__ && __STDC_VERSION__ >= 202000L && ((defined __GNUC__ && __GNUC__ >= 10) || (defined __clang__ && __clang_major__ >= 9)))
      [[fallthrough]];
#elif (defined __GNUC__ && __GNUC__ >= 7) || (defined __clang__ && __clang_major__ >= 10)
      __attribute__ ((__fallthrough__));
#endif
      /*FALLTHROUGH*/
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

const struct keyword_token *
is_keyword (register const char *str, register size_t len)
{
#if (defined __GNUC__ && __GNUC__ + (__GNUC_MINOR__ >= 6) > 4) || (defined __clang__ && __clang_major__ >= 3)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif
  static const struct keyword_token wordlist[] =
    {
      {""}, {""}, {""},
#line 68 "keywords.gperf"
      {"xor", XOR},
      {""}, {""}, {""},
#line 14 "keywords.gperf"
      {"or", OR},
#line 23 "keywords.gperf"
      {"out", OUT},
#line 31 "keywords.gperf"
      {"terminate", TERMINATE},
      {""}, {""},
#line 67 "keywords.gperf"
      {"reverse", REVERSE},
#line 47 "keywords.gperf"
      {"use", USE},
#line 36 "keywords.gperf"
      {"else", ELSE},
#line 45 "keywords.gperf"
      {"entry", ENTRY},
#line 61 "keywords.gperf"
      {"for", FOR},
      {""}, {""},
#line 60 "keywords.gperf"
      {"case", CASE},
#line 9 "keywords.gperf"
      {"of", OF},
      {""},
#line 35 "keywords.gperf"
      {"at", AT},
#line 62 "keywords.gperf"
      {"not", NOT},
#line 34 "keywords.gperf"
      {"then", THEN},
#line 50 "keywords.gperf"
      {"range", RANGE},
#line 63 "keywords.gperf"
      {"return", RETURN},
      {""},
#line 39 "keywords.gperf"
      {"elsif", ELSIF},
#line 27 "keywords.gperf"
      {"task", TASK},
#line 6 "keywords.gperf"
      {"abort", ABORT},
#line 16 "keywords.gperf"
      {"accept", ACCEPT},
      {""},
#line 64 "keywords.gperf"
      {"constant", CONSTANT},
#line 49 "keywords.gperf"
      {"exception", EXCEPTION},
#line 32 "keywords.gperf"
      {"array", ARRAY},
#line 10 "keywords.gperf"
      {"select", SELECT},
#line 58 "keywords.gperf"
      {"renames", RENAMES},
      {""},
#line 54 "keywords.gperf"
      {"when", WHEN},
      {""},
#line 20 "keywords.gperf"
      {"access", ACCESS},
#line 25 "keywords.gperf"
      {"do", DO},
#line 11 "keywords.gperf"
      {"abs", ABS},
#line 38 "keywords.gperf"
      {"type", TYPE},
      {""},
#line 53 "keywords.gperf"
      {"record", RECORD},
#line 7 "keywords.gperf"
      {"declare", DECLARE},
      {""},
#line 51 "keywords.gperf"
      {"exit", EXIT},
#line 46 "keywords.gperf"
      {"raise", RAISE},
      {""},
#line 30 "keywords.gperf"
      {"package", PACKAGE},
#line 57 "keywords.gperf"
      {"new", NEW},
#line 41 "keywords.gperf"
      {"procedure", PROCEDURE},
      {""}, {""},
#line 29 "keywords.gperf"
      {"is", IS},
#line 15 "keywords.gperf"
      {"separate", SEPARATE},
#line 13 "keywords.gperf"
      {"goto", GOTO},
#line 22 "keywords.gperf"
      {"if", IF},
#line 65 "keywords.gperf"
      {"function", FUNCTION},
#line 19 "keywords.gperf"
      {"subtype", SUBTYPE},
#line 24 "keywords.gperf"
      {"all", ALL},
#line 66 "keywords.gperf"
      {"null", NuLL},
#line 56 "keywords.gperf"
      {"while", WHILE},
      {""},
#line 26 "keywords.gperf"
      {"in", IN},
      {""},
#line 43 "keywords.gperf"
      {"loop", LOOP},
#line 12 "keywords.gperf"
      {"delay", DELAY},
      {""}, {""},
#line 42 "keywords.gperf"
      {"end", END},
#line 59 "keywords.gperf"
      {"with", WITH},
      {""},
#line 18 "keywords.gperf"
      {"others", OTHERS},
      {""}, {""}, {""},
#line 17 "keywords.gperf"
      {"delta", DELTA},
      {""},
#line 8 "keywords.gperf"
      {"generic", GENERIC},
#line 55 "keywords.gperf"
      {"rem", REM},
      {""}, {""},
#line 33 "keywords.gperf"
      {"pragma", PRAGMA},
      {""}, {""}, {""}, {""}, {""},
#line 37 "keywords.gperf"
      {"private", PRIVATE},
#line 28 "keywords.gperf"
      {"and", AND},
#line 48 "keywords.gperf"
      {"body", BODY},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 21 "keywords.gperf"
      {"digits", DIGITS},
#line 40 "keywords.gperf"
      {"limited", LIMITED},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 52 "keywords.gperf"
      {"mod", MOD},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 44 "keywords.gperf"
      {"begin", BEGiN}
    };
#if (defined __GNUC__ && __GNUC__ + (__GNUC_MINOR__ >= 6) > 4) || (defined __clang__ && __clang_major__ >= 3)
#pragma GCC diagnostic pop
#endif

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register unsigned int key = hash_keyword (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          register const char *s = wordlist[key].name;

          if ((((unsigned char)*str ^ (unsigned char)*s) & ~32) == 0 && !gperf_case_strncmp (str, s, len) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return (struct keyword_token *) 0;
}
