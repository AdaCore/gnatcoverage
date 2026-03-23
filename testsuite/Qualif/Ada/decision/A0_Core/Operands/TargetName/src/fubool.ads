package FUBOOL is


   function Eval (R1, R2 : Integer) return Boolean;
   -- Whether R1 < R2

   R1 : Integer := 1;

   R2_F : Integer := 0;
   R2_T : Integer := 10;
end;
