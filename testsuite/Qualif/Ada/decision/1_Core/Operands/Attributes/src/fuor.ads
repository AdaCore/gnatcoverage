package Fuor is

   function Eval (R1, R2 : String) return Boolean;
   -- Whether R2 fits in R1, as
   -- R2'Length < R1'Length or else R2'Length = R1'Length

   R1 : String := "12345";

   R2_FF : String := "1234567";
   R2_FT : String := "12345";
   R2_TX : String := "123";
end;



