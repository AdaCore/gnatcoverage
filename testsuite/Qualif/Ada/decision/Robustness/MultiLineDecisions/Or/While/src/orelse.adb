function Orelse (A, B : Boolean) return Boolean is
begin
   while A      -- # eval0
     or else B  -- # eval1
   loop
      return True;  -- # true
   end loop;

   return False; -- # false
end;


