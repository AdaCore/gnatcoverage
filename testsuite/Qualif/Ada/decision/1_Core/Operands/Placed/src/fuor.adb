package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
   begin
      if Ops.A > 0 or else Ops.B > 0 then -- # eval0
         return True; -- # true
      else
         return False; -- # false
      end if;
   end;
end;
