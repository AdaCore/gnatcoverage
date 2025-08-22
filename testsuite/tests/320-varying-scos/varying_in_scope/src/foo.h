int
foo ()
{
#ifdef A
  return 0;
#endif
#ifdef B
  return 1;
#endif;
}
