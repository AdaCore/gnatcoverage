#include <span>
#include <string>
#include <vector>

extern std::vector<std::string> add_dashes (std::string name);

extern bool read_flag (std::span<std::string_view> &args, std::string name);
