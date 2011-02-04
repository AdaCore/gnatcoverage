
package body A1O1 is

   function First_Of (A, B : Boolean) return Boolean is
   begin
      return A;
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      --  mutliple (and different) decisions over the same arguments on the
      --  same line below:
      return First_Of (A and then B, A or else B); -- # evals
   end;
end;
