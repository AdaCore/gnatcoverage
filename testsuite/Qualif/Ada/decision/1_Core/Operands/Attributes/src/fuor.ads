package Fuor is

   type String_Access is access String;

   function Eval (R1, R2 : String_Access) return Boolean;
   -- Whether R2 fits in R1, as
   -- R2'Length < R1'Length or else R2'Length = R1'Length

   R1 : String_Access := new String'("12345");

   R2_FF : String_Access := new String'("1234567");
   R2_FT : String_Access := new String'("12345");
   R2_TX : String_Access := new String'("123");
end;



