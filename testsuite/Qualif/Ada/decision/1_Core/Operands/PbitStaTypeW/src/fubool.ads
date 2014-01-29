package FUBOOL is
   
   type Bit is mod 2;
   type Bitmap is array (1 .. 1024) of Bit;
   pragma Pack (Bitmap);
   
   Data : Bitmap := (1 .. 512 => 0, 513 .. 1024 => 1);
   
   function Eval (R1, R2 : Integer) return Boolean;
   -- Whether data(R1 + R2) = 1

   R1 : Integer := 1;

   R2_F : Integer := 0;    -- data(1+0) = 0
   R2_T : Integer := 600;  -- data(1+600) = 1

end;
