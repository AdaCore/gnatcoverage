function Orelse (A, B : Boolean) return Boolean is
begin
   if A or else B then  -- # eval
      return True;       -- # true
   else
      return False;      -- # false
   end if;
end;
