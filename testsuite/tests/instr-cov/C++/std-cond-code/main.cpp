void
print_msg (const char *)
{
}

int
main ()
{
#if defined(__STDC_VERSION__)
  // This is true if the code is preprocessed as C code
  print_msg ("This is C code");
#elif __cplusplus == 202002L
  // This is true if the code is preprocessed as C++20 code
  print_msg ("This is C++20 code");
#else
  print_msg ("This is C++ code");
#endif
  return 0;
}
