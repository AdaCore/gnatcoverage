package FUAND is
   
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
     (1 .. 4   => (Bit0 => 0, Bit1 => 0),
      5 .. 8   => (Bit0 => 0, Bit1 => 1),
      9 .. 12  => (Bit0 => 1, Bit1 => 0),
      13 .. 16 => (Bit0 => 1, Bit1 => 1)
     );
   
   function Eval (R1, R2 : Integer) return Boolean;
   -- data(R1+R2).bit0 is set and then data(R1+R2).bit1 is set

   R1 : Integer := 1;

   R2_FX : Integer := 0;
   R2_TF : Integer := 9;

   R2_TT : Integer := 14;
end;
