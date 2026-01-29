long long operator""_K(unsigned long long n) { // # func-k
  return n * 1024;
}

long long operator""_M(unsigned long long n) { // # func-m
  return n * 1024 * 1024;                      // # no-cov
}

void kilo(void) {
  // Douze kilometres a pieds...
  int twelve_k = 12_K; // # call-k
}

void mega(void) {      // # func-mega
  int twelve_m = 12_M; // # call-m
}

int main(void) {
  // Only call kilo
  kilo();
}

//# test_int_literal.cpp
// /func-k/     l+ ## 0
// /func-m/     l- ## f-
// /no-cov/     l- ## s-
// /call-k/     l+ ## 0
// /funk-mega/  l- ## f-
// /call-m/     l- ## s-,c-
