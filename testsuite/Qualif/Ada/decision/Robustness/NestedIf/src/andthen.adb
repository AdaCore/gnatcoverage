function Andthen (A, B : Boolean) return Boolean is
begin
   if A then          -- # evalA
      if B then       -- # evalB
         return True; -- # true
      end if;
   end if;
   return False;      -- # false
end;
