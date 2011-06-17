package body Evals is

   procedure Eval (A, B : Boolean) is
      VA, VB : Boolean;
      pragma Volatile (VA);
      pragma Volatile (VB);
   begin
      VA := A;
      VB := B;
   end;
end;

