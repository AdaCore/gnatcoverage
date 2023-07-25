package Pkg is

   type Arr_T is array (Positive range <>) of Integer;

   type Op_T is (Sum, Abs_Mult);

   function Compute (Inp : Arr_T; Op : Op_T) return Integer;
   --  If Op is Sum, return the sum of the elements in Inp. otherwise, if Op
   --  is Abs_Mult, return the product of the absolute value of the non-zero
   --  elements in Inp.
end Pkg;
