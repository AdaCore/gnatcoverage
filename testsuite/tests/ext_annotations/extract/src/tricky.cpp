// Check that annotations are still found past lexical constructs that a
// hand-written scanner would trip on: a C++ raw string literal holding an
// unbalanced double quote and a comment opener.

const char *tricky_text = R"(a " b /* not a comment */)";

/* GNATCOV_EXEMPT_ON ("never called") */
int
tricky (int x)
{
  if (x < 0)
    return -1;
  return 0;
}
/* GNATCOV_EXEMPT_OFF */
