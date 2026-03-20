class Number {
  int n;

public:
  Number(int n) : n(n){}; // # ctor
  Number() : n(-1) {} // # ctor
};

void not_called(void) { // # func
  auto a = Number(5); // # call

  // This makes a CXXConstructExpr, which calls the copy constructor
  auto b(a); // # limitation
}

int main(void) {
  // Do nothing
}

//# test_no_call.cpp
//  /ctor/       l- ## f-
//  /func/       l- ## f-
//  /call/       l- ## s-,c-
//  /limitation/ l- ## s-,c?
