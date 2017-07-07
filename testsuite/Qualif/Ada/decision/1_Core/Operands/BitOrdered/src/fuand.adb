package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
   begin
      if Ops.A and then Ops.B then -- # eval0
         return True; -- # true
      else 
         return False; -- # false
      end if;         
   end;
end;
