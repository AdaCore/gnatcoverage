package pkg is

   type Arr_T is array (Positive range <>) of Integer;

   function Non_Zero_Mult (Arr : Arr_T) return Integer;
   --  Returns the product of all the non-zero elements in Arr

end pkg;
