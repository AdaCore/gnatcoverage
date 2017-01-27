package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
   begin
      if Ops.A = TT or else Ops.B = TT then -- # eval0
         return True; -- # true
      else 
         return False; -- # false
      end if;
   end;

end;
