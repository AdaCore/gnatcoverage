#include "cmdline.hh"

std::vector<std::string>
add_dashes (std::string name)
{
  // Multi-letter linker options can be preceded by either a single
  // dash or double dashes except ones starting with "o", which must
  // be preceded by double dashes. For example, "-omagic" is
  // interpreted as "-o magic". If you really want to specify the
  // "omagic" option, you have to pass "--omagic".
  if (name[0] == 'o')
    return { "--" + name };
  return { "-" + name, "--" + name };
}

bool
read_flag (std::span<std::string_view> &args, std::string name)
{
  for (std::string opt : add_dashes (name))
    {
      if (args[0] == opt)
        {
          args = args.subspan (1);
          return true;
        }
    }
  return false;
}
