#include <iostream>

extern "C" {
  extern void cpp_func (void);
}

void
cpp_func (void)
{
  std::cout << "cpp:cpp_func" << std::endl;
}
