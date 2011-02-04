package body A1O2 is

   function First_Of (X, Y : Boolean) return Boolean is
   begin
      return X;
   end;

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return First_Of (A and then B, C or else D); -- # evals
   end;
end;

