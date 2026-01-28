unsigned long operator""_len(const char *s, unsigned long n) { // # func-len
  return n;                                                    // # no-cov
}

void len(void) {            // # func-len
  int twelve_k = "xxx"_len; // # call-len
}

int main(void) {
  // Nothing to do
}

//# test_str_literal.cpp
//  /func-len/   l- ## f-
//  /no-cov/     l- ## s-
//  /funk-len/   l- ## f-
//  /call-len/   l- ## s-,c-
