package FUBOOL is
   
   type Bit is mod 2;
   type Bitmap is array (1 .. 32) of Bit;
   pragma Pack (Bitmap);
   
   Data : Bitmap := 
     (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  -- indexes 1 to 16
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1   -- indexes 17 to 32
     );
   
   function Eval (R1, R2 : Integer) return Boolean;
   -- Whether data(R1 + R2) = 1

   R1 : Integer := 1;

   R2_F : Integer := 0;   -- data(1+0) = 0
   R2_T : Integer := 17;  -- data(1+17) = 1

end;
