function Check (A, B, C : Boolean) return Boolean is
begin
   return A and then not (B and then C);
end;

