#include <span>

int
main (int argc, char **argv)
{
  int a[]{ 0, 1, 2 };
  std::span<int> s_all (a);
  std::span<int> s_0 = s_all.subspan (0, 1);
  std::span<int> s_2 = s_all.subspan (2, 1);

  if (s_0[0] != 0)
    return 1;
  if (s_2[0] != 2)
    return 1;

  return 0;
}
