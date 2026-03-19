#include "c_print.h"

void
unit_c_skipped (void)
{
  print_msg ("Start unit");
  /* GNATCOV_RESET_BUFFERS */
  print_msg ("End unit");
}
