with Harness;
with Mylib;

procedure Test2 is
begin
   Harness.Assert_Equal ("Prev (1)", 0, Mylib.Prev (1));
   Harness.Assert_Equal ("Prev (2)", 1, Mylib.Prev (2));
end Test2;
