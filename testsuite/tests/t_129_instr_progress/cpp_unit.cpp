extern "C"
{
  extern void print_msg (const char *);
  extern void cpp_func (void);
}

void
cpp_func (void)
{
  print_msg ("cpp:cpp_func");
}
