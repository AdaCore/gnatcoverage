package FUAND is
   
   type Bit_Pair is record
      Bit0, Bit1 : Boolean;
   end record;
   
   Values : array (0 .. 3) of Bit_Pair := 
     (0 => (Bit1 => False, Bit0 => False),
      1 => (Bit1 => False, Bit0 => True),
      2 => (Bit1 => True, Bit0 => False),
      3 => (Bit1 => True, Bit0 => True));
      
   function Eval (R1, R2 : Integer) return Boolean;

   -- Whether Values(R1+R2).Bit0 and then Values(R1+R2).Bit1


   R1 : Integer := 0;

   R2_FX : Integer := 0; 
   R2_TF : Integer := 1;

   R2_TT : Integer := 3;
end;
