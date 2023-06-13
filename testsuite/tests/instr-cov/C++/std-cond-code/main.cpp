#include <iostream>

int
main ()
{
#if defined(__STDC_VERSION__)
  // This is true if the code is preprocessed as C code
  std::cout << "This is C code" << std::endl;
#elif __cplusplus == 202002L
  // This is true if the code is preprocessed as C++20 code
  std::cout << "This is C++20 code" << std::endl;
#else
  std::cout << "This is C++ code" << std::endl;
#endif
  return 0;
}
