extern int cpp_func (int a);

int
c_func (int a, int b)
{
  return cpp_func (a) + cpp_func (b);
}
