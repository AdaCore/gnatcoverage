
package body A1A2 is

   function Andthen (A, B : Boolean) return Boolean is
   begin
      return A and then B; -- # valueF
   end;

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return Andthen (A and then B, C and then D); -- # evals
   end;
end;
