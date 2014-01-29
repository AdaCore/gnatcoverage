package FUBOOL is
   
   type Bitmap is array (1 .. 32) of Boolean;
   pragma Pack (Bitmap);
   
   Data : Bitmap :=  (1 .. 16 => False, 17 .. 32 => True);
   
   function Eval (R1, R2 : Integer) return Boolean;
   -- Whether data(R1 + R2) = True

   R1 : Integer := 1;

   R2_F : Integer := 0;   -- data(1+0) = False
   R2_T : Integer := 17;  -- data(1+17) = True

end;
