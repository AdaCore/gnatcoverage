class Number {
  int n;

public:
  Number(int n) : n(n){}; // # ctor
  Number() : n(-1) {} // # ctor
};

void not_called(void) { // # func
  // This makes a CXXConstructExpr, which calls the constructor
  auto a = Number(5); // # call

  Number d, e, f(11); // # multiple

  auto c = a;            // # call
  auto x = (nullptr, a); // # call

  // Current limitation. See test 382-vardecl_callinit_auto
  // auto b(a);
}

int main(void) {
  // Do nothing
}

//# test_construct_expr.cpp
//  /ctor/      l- ## f-
//  /func/      l- ## f-
//  /call/      l- ## s-,c-
//  /multiple/  l- ## s-,c-,c-,c-
