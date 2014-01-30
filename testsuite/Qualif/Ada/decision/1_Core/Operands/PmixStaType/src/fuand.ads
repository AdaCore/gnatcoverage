package FUAND is
   
   type Bitmap is array (1 .. 32) of Boolean;
   pragma Pack (Bitmap);
   
   Data : Bitmap :=  (1 .. 16 => False, 17 .. 32 => True);
   
   function Eval (R1, R2 : Integer) return Boolean;
   -- R1 + R2 is even and then data(R1 + R2)

   R1 : Integer := 1;

   R2_FX : Integer := 2;
   R2_TF : Integer := 1;

   R2_TT : Integer := 17;
end;
