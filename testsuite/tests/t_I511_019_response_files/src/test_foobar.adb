with Foo; with Bar;

procedure Test_Foobar is
begin
   Foo;
   Bar.Proc1;
   Bar.Proc2;

   --  This test is designed for command line checks,
   --  not to be run

   raise Program_Error;
end;
