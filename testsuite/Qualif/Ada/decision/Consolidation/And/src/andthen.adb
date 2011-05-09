function Andthen (A, B : Boolean) return Boolean is
begin
   if A and then B then  -- # eval
      return True;       -- # true
   else
      return False;      -- # false
   end if;
end;
