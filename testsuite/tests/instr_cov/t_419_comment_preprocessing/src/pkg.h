#define CONCAT(X, Y) X##Y

inline int
ident (int x)
{
  /* GNATCOV_EXEMPT_ON unused*/
  return 0; // # noeval
  /*GNATCOV_EXEMPT_OFF */
}
