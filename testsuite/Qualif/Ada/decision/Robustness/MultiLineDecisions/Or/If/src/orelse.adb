function Orelse (A, B : Boolean) return Boolean is
begin
   if A         -- # eval0
     or else B  -- # eval1
   then
      return True;  -- # true
   else
      return False; -- # false
   end if;
end;
