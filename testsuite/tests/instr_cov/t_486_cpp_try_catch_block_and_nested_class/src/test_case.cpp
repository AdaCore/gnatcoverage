#include <iostream>
#include <stdexcept>

int
main ()
{
  class Exceptional
  {
  public:
    void first () {                                 // # ffirst
      throw std::runtime_error ("First Exception"); // # ffirst
    }

    void second () {                                 // # fsecond-1
      throw std::runtime_error ("Second Exception"); // # fsecond-2
    }
  };

  Exceptional obj; // # obj
  try
    {
      obj.first ();  // # call_first
      obj.second (); // # call_second
    }
  catch (const std::exception &e)
    {
      std::cerr << "Caught \"" << e.what () << "\"" << std::endl; // # catch
    }
  return 0; // # return
}

//# test_case.cpp
//
// /ffirst/         l+ ## 0
//
// /fsecond/        l- ## 0
// /fsecond-1/      l= ## f-
// /fsecond-2/      l= ## s-,c-
//
// /obj/            l+ ## 0
// /call_first/     l+ ## 0
// /call_second/    l- ## s-,c-
// /catch/          l+ ## 0
// /return/         l+ ## 0
//
// %tags:block
// =/obj/           l! ## s-
// =/call_first/    l! ## s-
