package FUBOOL is
   
   type Bit is mod 2;
   type Bitmap is array (Natural range <>) of Bit;
   pragma Pack (Bitmap);
   
   Data : Bitmap := (1 .. 16 => 0, 17 .. 32 => 1);
   
   function Eval (R1, R2 : Integer) return Boolean;
   -- Whether data(R1 + R2) = 1

   R1 : Integer := 1;

   R2_F : Integer := 0;   -- data(1+0) = 0
   R2_T : Integer := 17;  -- data(1+17) = 1

end;
