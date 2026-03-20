class Number {
  int n;

public:
  Number(int n) : n(n){}; // # ctor
};

void not_called(void) { // # func
  // This makes a CXXTemporaryObjectExpr, which calls the constructor
  auto number = Number{5}; // # call
}

int main(void) {
  // Do nothing
}

//# test_tmp_obj_expr.cpp
//  /ctor/   l- ## f-
//  /func/   l- ## f-
//  /call/   l- ## s-,c-
