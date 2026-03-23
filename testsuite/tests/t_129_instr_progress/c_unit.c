extern void cpp_func (void);
extern void print_msg (const char *);

void
c_func (void)
{
  print_msg ("c:c_func");
  cpp_func ();
}
