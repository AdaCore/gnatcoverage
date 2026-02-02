unsigned long operator""_len(const char *s, __SIZE_TYPE__ n) { // # func-len
  return n;                                                    // # no-cov
}

void len(void) {            // # func-len
  int twelve_k = "xxx"_len; // # call-len
}

int main(void) {
  // Do nothing
}

//# test_str_literal.cpp
//  /func-len/   l- ## f-
//  /no-cov/     l- ## s-
//  /call-len/   l- ## s-,c-
