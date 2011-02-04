
package body A1O1 is

   function Andthen (A, B : Boolean) return Boolean is
   begin
      return A and then B; -- # valueF
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      return Andthen (A and then B, A or else B); -- # evals
   end;
end;
