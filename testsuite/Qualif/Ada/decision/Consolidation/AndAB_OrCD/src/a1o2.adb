package body A1O2 is
   function Andthen (A, B : Boolean) return Boolean is
   begin
      if A and then B then -- # valueF
         return True;      -- # true
      else
         return False;     -- # false
      end if;
   end;

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return Andthen (A and then B, C or else D); -- # evals
   end;
end;


