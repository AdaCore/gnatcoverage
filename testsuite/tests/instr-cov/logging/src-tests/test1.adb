with Harness;
with Mylib;

procedure Test1 is
begin
   Harness.Assert_Equal ("Next (0)", 1, Mylib.Next (0));
   Harness.Assert_Equal ("Next (1)", 2, Mylib.Next (1));
end Test1;
