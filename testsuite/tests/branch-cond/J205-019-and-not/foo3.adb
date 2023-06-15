function Foo3 (A, B, C : Boolean) return Boolean is
begin
   return A and then not B and then not C;
end;

