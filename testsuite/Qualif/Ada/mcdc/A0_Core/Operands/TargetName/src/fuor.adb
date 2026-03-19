pragma Ada_2022;

package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
   begin
      return X : Boolean := Ops.A do  -- # decl
        X := @ -- # evalTN
            or else Ops.B; -- # evalB
      end return;
   end;
end;
