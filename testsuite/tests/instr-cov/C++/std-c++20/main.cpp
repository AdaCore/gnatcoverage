#include "cmdline.hh"

int
main (int argc, char **argv)
{
  std::vector<std::string_view> args_vec (argv, argv + argc);
  std::span<std::string_view> args = args_vec;
  read_flag (args, "output");
  read_flag (args, "version");
}
