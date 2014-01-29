package FUAND is
   
   type Bitmap is array (1 .. 32) of Boolean;
   pragma Pack (Bitmap);
   
   Data : Bitmap :=  (1 .. 16 => False, 17 .. 32 => True);
   
   type XY is record
      X, Y : Integer;
   end record;
   
   function Eval (R1, R2 : XY) return Boolean;
   -- data(R1.X + R2.X) and then data(R1.Y + R2+Y)

   R1 : XY := (X => 1, Y => 1);

   R2_FX : XY := (X => 0, Y => 9);    -- data(1+0) = True
   R2_TF : XY := (X => 17, Y => 1);   -- data(1+17) = True, data(1+1) = False

   R2_TT : XY := (X => 17, Y => 20);  -- data(1+17) = True, data(1+20) = True
end;
