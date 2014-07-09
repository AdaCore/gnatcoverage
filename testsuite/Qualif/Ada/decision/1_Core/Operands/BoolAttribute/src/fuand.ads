with Ada.Unchecked_Conversion;

package FUAND is
   
   type Rint is new Integer range 1 .. 5;
   for Rint'Size use 32;
   
   function To_Rint is new Ada.Unchecked_Conversion (Integer, Rint);
   
   Values : array (1 .. 3) of Rint := 
     (1 => To_Rint(1),   -- valid
      2 => To_Rint(999), -- invalid
      3 => To_Rint(1)    -- valid
     ); 
      
   function Eval (R1, R2 : Integer) return Boolean;
   -- Whether R1+R2 > 1 and then Values(R1+R2)'Valid

   R1 : Integer := 1;

   R2_FX : Integer := 0; 
   R2_TF : Integer := 1;
   R2_TT : Integer := 2;
end;
