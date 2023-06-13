#include <iostream>

extern void shared();

inline void shared() {
    std::cout << "Hello, world!" << std::endl;
}
