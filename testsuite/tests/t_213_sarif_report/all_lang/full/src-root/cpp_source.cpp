extern "C"
{
  extern int cpp_func (int a);
}

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
cpp_func (int a)
{
  bool t = true;
  bool f = false;

  //  DECISISON violation
  if (!t)
    {
      //  STMT violation
      t = false;
    }

  MCDC_Violation (f, f);
  MCDC_Violation (t, f);

  /* GNATCOV_EXEMPT_ON "cpp justification"*/
  if (!t)
    {
      t = false;
    }
  /* GNATCOV_EXEMPT_OFF */

  return a;
}
