package FUBOOL is
   
   type Bit4 is mod 2 ** 4;
   for Bit4'Size use 4;
   
   type Bitmap is array (1 .. 8) of Bit4;
   pragma Pack (Bitmap);
   
   Data : Bitmap := (1 .. 4 => 0, 5 .. 8 => 7);
   
   function Eval (R1, R2 : Integer) return Boolean;
   -- Whether data(R1 + R2) > 5

   R1 : Integer := 1;

   R2_F : Integer := 0;  -- data(1+0) = 0
   R2_T : Integer := 6;  -- data(1+6) = 7

end;
