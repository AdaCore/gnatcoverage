#include "count_digits.hh"

int
count_digits (const char *str)
{
  int result = 0;               // # init
  while (const char c = *str++) // # while-cond
    {
      if ('0' <= c && c <= '9') // # if-cond
        result += 1;            // # if-incr
    }
  return result; // # return
}
