package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
   begin
      if Ops.A = TT and then Ops.B = TT then -- # eval0
         return True; -- # true
      else
         return False; -- # false
      end if;
   end;
end;
