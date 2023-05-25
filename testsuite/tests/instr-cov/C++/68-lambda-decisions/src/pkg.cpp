#include "pkg.hh"

int
overly_complex_fact (int n)
{
  return n ? n * // # top_level
	       [] (int m) {
		 return m > 1 ? overly_complex_fact (m) : 1; // # lambda
	       }(n - 1)
	   : 1;
}
