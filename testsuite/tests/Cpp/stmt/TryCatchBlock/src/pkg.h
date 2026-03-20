#include <stdexcept>

class custom_exception : public std::runtime_error
{
public:
  custom_exception () : runtime_error ("custom exception") {}
};

void catch_not_positive (int i);
