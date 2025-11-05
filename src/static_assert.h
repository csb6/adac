#ifndef ADA_STATIC_ASSERT_H
#define ADA_STATIC_ASSERT_H
// Taken from https://www.pixelbeat.org/programming/gcc/static_assert.html and modified
// to be usable in a top-level context
/*
Copyright 2008, Pádraig Brady

Copying and distribution of this file, with or without modification, are permitted in
any medium without royalty, provided the copyright notice and this notice are preserved.
This file is offered as-is, without any warranty.
*/
#define ASSERT_CONCAT_(a, b) a##b
#define ASSERT_CONCAT(a, b) ASSERT_CONCAT_(a, b)
/* These can't be used after statements in c89. */
#ifdef __COUNTER__
  #define STATIC_ASSERT(e) \
    enum { ASSERT_CONCAT(static_assert_, __COUNTER__) = 1/(int)(!!(e)) }
#else
  /* This can't be used twice on the same line so ensure if using in headers
   * that the headers are not included twice (by wrapping in #ifndef...#endif)
   * Note it doesn't cause an issue when used on same line of separate modules
   * compiled with gcc -combine -fwhole-program.  */
  #define STATIC_ASSERT(e) \
    enum { ASSERT_CONCAT(assert_line_, __LINE__) = 1/(int)(!!(e)) }
#endif
#endif /* ADA_STATIC_ASSERT_H */
