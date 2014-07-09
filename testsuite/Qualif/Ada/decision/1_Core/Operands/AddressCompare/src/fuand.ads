package FUAND is
   
   Data : array (1 .. 32) of Integer := (others => 0);
   
   function Eval (R1, R2 : Integer) return Boolean;

   -- Whether R1+R2 > 12 and then data(R1)'address < data(R2)'address

   R1 : Integer := 8;

   R2_FX : Integer := 1; 
   R2_TF : Integer := 5; 

   R2_TT : Integer := 9;
end;
