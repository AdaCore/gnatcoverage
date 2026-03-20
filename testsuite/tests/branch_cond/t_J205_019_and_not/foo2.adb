function Foo2 (A, B, C : Boolean) return Boolean is
begin
   return not A and then (B or else C);
end;
