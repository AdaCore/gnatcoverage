pragma Ada_2022;

package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
   begin
      return (declare OA : Boolean renames Ops.A; begin OA) -- # evalA
        and then (declare OB : constant Boolean := Ops.B; begin OB); -- # evalB
   end;
end;
