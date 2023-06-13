function Foo (A, B, C, D : Boolean) return Boolean is
begin
   return A and then B and then (not C and then not D);
end;

