function F (A, B, C, D : Boolean) return Boolean is
begin
   if (A and then B) or (C or D) then
      return True;
   else
      return False;
   end if;
end;
