function And_Eq_Or (A, B, C, D : Boolean) return Boolean is
begin
   if ((A and then B) = (C or else D)) then -- # eval
      return True;   -- # true
   else
      return False;  -- # false
   end if;
end;

