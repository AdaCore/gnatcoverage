package FUAND is
   
   subtype RangeP is Integer range 10 .. 15;
   subtype RangeM is Integer range 1 .. 5;
   
   function Eval (R1, R2 : Integer) return Boolean;

   -- Whether R1+R2 is in RangeP and then R1-R2 is in RangeM


   R1 : Integer := 7;

   R2_FX : Integer := 1; -- 7+1=8 not in RangeP
   R2_TF  : Integer := 7; -- 7+7=14, in Range P; 7-7=0, not in RangeM

   R2_TT : Integer := 3;  -- 7+3=10, in RangeP; 7-3=4, in RangeM
end;
