pragma Ada_2022;

with Support; use Support;

package body FUAND is

   function Eval (R1, R2 : XYrange) return Boolean is
   begin
      if (declare R2X : constant Integer := R2.X; begin R2X <= R1.Y)  -- # eval0
        and then (declare R1X : Integer renames R1.X; begin R2.Y > R1X)  -- # eval1
      then
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end Eval;

end FUAND;
