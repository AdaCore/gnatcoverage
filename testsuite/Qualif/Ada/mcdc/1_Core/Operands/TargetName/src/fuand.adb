pragma Ada_2022;

package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
   begin
      return X : Boolean := Ops.A do  -- # decl
        X := @ -- # evalTN
            and then Ops.B; -- # evalB
      end return;
   end;
end;
