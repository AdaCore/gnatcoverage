package Pkg is

   type Arr_T is array (Positive range <>) of Integer;

   type Op_T is (Sum, Max);

   function Compute (Inp : Arr_T; Op : Op_T) return Integer;
   --  Compute either the sum or the maximum of the Element in Inp based on the
   --  value of Op.

end Pkg;
