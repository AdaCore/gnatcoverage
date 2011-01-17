
package body A1O2 is

   function Andthen (A, B : Boolean) return Boolean is
   begin
      return A and then B; -- # valueF
   end;

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return Andthen (A and then B, C or else D); -- # evals
   end;
end;
