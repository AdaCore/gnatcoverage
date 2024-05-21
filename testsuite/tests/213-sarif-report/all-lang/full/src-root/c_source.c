#include <stdbool.h>

extern int cpp_func (int a);

bool
MCDC_Violation (bool t, bool f)
{
  if ((t || f) && (t || f))
    {
      return true;
    }

  return true;
}

int
c_func (int a, int b)
{

  bool t = true;
  bool f = false;

  //  DECISISON violation
  if (!t)
    {
      //  STMT violation
      t = false;
    }

  t = MCDC_Violation (f, f);
  t = MCDC_Violation (t, f);

  /* GNATCOV_EXEMPT_ON "c_justification"*/
  if (!t)
    {
      t = false;
    }
  /* GNATCOV_EXEMPT_OFF */

  return cpp_func (a) + cpp_func (b);
}
