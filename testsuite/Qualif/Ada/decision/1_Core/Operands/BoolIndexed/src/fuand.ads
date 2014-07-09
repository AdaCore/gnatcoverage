package FUAND is
   
   type Bitmap is array (1 .. 32) of Boolean;
   
   BM1 : Bitmap := (1 .. 16  => False,
                    17 .. 32 => True);
   -- 00000000000000001111111111111111
   
   BM2 : Bitmap := (1 .. 8  => False,
                    9 .. 16 => True,
                    17 .. 24 => False,
                    25 .. 32 => True);
   -- 00000000111111110000000011111111
   
   function Eval (R1, R2 : Integer) return Boolean;
   
   -- BM1(R1+R2) and then BM2(R1+R2)


   R1 : Integer := 1;

   R2_FX : Integer := 0; 
   R2_TF : Integer := 15; 

   R2_TT : Integer := 30;
end;
