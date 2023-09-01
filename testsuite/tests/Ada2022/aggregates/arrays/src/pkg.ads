package Pkg is

   type Int_Arr is array (Positive range <>) of Integer;

   function Absolute (Input : Int_Arr) return Int_Arr;

end Pkg;
