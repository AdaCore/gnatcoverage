package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
   begin
      if Ops(Op_A) > 0.0 or else Ops(Op_B) > 0.0 then -- # eval0
         return True; -- # true
      else
         return False; -- # false;
      end if;
   end;
end;
