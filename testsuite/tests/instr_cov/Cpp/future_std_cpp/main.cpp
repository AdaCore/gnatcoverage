#include <stdfloat>

int
main (int argc, char **argv)
{
  std::bfloat16_t x = 2.0bf16;
  std::bfloat16_t y = 2.0bf16;

  return x - y == 0.0 ? 0 : 1;
}
