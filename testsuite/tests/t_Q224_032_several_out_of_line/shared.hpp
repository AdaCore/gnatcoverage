extern "C" void bar();
extern void shared();

inline void shared() {
    bar();
}
