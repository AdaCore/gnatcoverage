pragma Ada_2022;

package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
   begin
      return (declare OA : Boolean renames Ops.A; begin OA) -- # evalA
        or else (declare OB : constant Boolean := Ops.B; begin OB); -- # evalB
   end;
end;
