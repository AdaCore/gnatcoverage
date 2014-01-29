package FUBOOL is
   
   type Bit is mod 2;
   for Bit'Size use 1;
   
   type Bits is record
      Bit0, Bit1 : Bit;
   end record;
   for Bits'Size use 2;
   pragma Pack (Bits);
   
   type Bitmap is array (1 .. 16) of Bits;
   pragma Pack (Bitmap);
   
   Data : Bitmap := 
     (1 .. 8   => (Bit0 => 0, Bit1 => 0),
      9 .. 16  => (Bit0 => 1, Bit1 => 0)
     );
   
   function Eval (R1, R2 : Integer) return Boolean;
   -- Whether data(R1 + R2).bit0 = 1

   R1 : Integer := 1;

   R2_F : Integer := 0;
   R2_T : Integer := 10;

end;
