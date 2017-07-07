package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
   begin
      if Ops.A > 0 and then Ops.B > 0 then -- # eval0
         return True; -- # true
      else
         return False; -- # false
      end if;
   end;
end;
