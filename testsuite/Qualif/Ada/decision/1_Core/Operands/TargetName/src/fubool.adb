pragma Ada_2022;

with Support; use Support;

package body FUBOOL is

   function Eval (R1, R2 : Integer) return Boolean is
   begin
      return X : Boolean := R1 < R2 do     -- # decl
         X := (if @ then True else False); -- # eval0
      end return;
   end Eval;

end FUBOOL;
