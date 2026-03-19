#include <initializer_list>

int
main (void)
{
  int sum = 0;
  for (auto i : { 1, 2, 3 })
    {
      sum += i;
    }
  return 0;
}
