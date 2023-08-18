struct Foo {
  void bar (){
    return; // # member-fun
  }
};

int
main (void)
{
  struct Foo foo; // # decl
  foo.bar();      // # call
  return 0;       // # return
}

//# test_struct.cpp
//
//  /decl/       l+ ## 0
//  /member-fun/ l+ ## 0
//  /call/       l+ ## 0
//  /return/     l+ ## 0
