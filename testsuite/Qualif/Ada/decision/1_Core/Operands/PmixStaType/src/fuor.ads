package FUOR is
   
   type Bitmap is array (1 .. 32) of Boolean;
   pragma Pack (Bitmap);
   
   Data : Bitmap :=  (1 .. 16 => False, 17 .. 32 => True);
   
   function Eval (R1, R2 : Integer) return Boolean;
   --  data(R1 + R2) or else R1 + R2 is even

   R1 : Integer := 1;

   R2_FF : Integer := 2;
   R2_FT : Integer := 5;

   R2_TX : Integer := 17;
end;
