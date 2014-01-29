package FUAND is
   
   type Bit4 is mod 2 ** 4;
   for Bit4'Size use 4;
   
   type Bitmap is array (1 .. 8) of Bit4;
   pragma Pack (Bitmap);
   
   Data : Bitmap := (1 .. 4 => 0, 5 .. 8 => 7);
   
   type XY is record
      X, Y : Integer;
   end record;
   
   function Eval (R1, R2 : XY) return Boolean;
   -- data(R1.X + R2.X) > 5 and then data(R1.Y + R2+Y) > 5 
   
   R1 : XY := (X => 1, Y => 1);

   R2_FX : XY := (X => 0, Y => 1);    -- data(1+0) = 0
   R2_TF : XY := (X => 5, Y => 1);   -- data(1+5) = 7, data(1+1) = 0

   R2_TT : XY := (X => 5, Y => 6);  -- data(1+5) = 7, data(1+6) = 7
end;
