#include "calculations.h"

//  simple function performing some basic math operations
//  which allows a division by 0
int
calculations_problematic (int a, int b)
{
  int temp = 0;
  temp = a * b;
  temp = temp + a / b;
  return temp;
}

// same as previous function but prohibits the division
// by zero
int
calculations (int a, int b)
{
  int temp = 0;
  temp = a * b;
  if (b != 0)
    temp = temp + a / b;
  return temp;
}
