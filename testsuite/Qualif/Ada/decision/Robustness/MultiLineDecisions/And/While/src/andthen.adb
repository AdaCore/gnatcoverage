function Andthen (A, B : Boolean) return Boolean is
begin

   while A       -- # eval0
     and then B  -- # eval1
   loop
      return True;  -- # true
   end loop;

   return False; -- # false
end;


