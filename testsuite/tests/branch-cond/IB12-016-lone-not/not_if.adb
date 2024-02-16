function Not_If (A, B : Boolean) return Boolean is
begin
   if B then
      return A;
   else
      return not A;
   end if;
end;
