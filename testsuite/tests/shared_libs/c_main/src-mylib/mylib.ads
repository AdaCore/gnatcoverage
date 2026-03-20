package Mylib is

   function Fact (N : Natural) return Natural;
   pragma Export (C, Fact, "mylib__fact");

end Mylib;
