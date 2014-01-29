package FUAND is
   
   type Bit is mod 2;
   type Bitmap is array (Natural range <>) of Bit;
   pragma Pack (Bitmap);
   
   Data : Bitmap (1 .. 1024) := (1 .. 512 => 0, 513 .. 1024 => 1);
   
   type XY is record
      X, Y : Integer;
   end record;
   
   function Eval (R1, R2 : XY) return Boolean;
   -- data(R1.X + R2.X) = 1 and then data(R1.Y + R2+Y) = 1

   R1 : XY := (X => 1, Y => 1);

   R2_FX : XY := (X => 0, Y => 9);    -- data(1+0) = 0
   R2_TF : XY := (X => 520, Y => 1);  -- data(1+520) = 1, data(1+1) = 0

   R2_TT : XY := (X => 520, Y => 536);  -- data(1+17) = 1, data(1+536) = 1
end;
