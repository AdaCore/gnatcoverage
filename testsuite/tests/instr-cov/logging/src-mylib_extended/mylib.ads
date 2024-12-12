package Mylib is

   function Next (I : Integer) return Integer;
   function Prev (I : Integer) return Integer is (I - 1);

   function Identity (I : Integer) return Integer
   with Import, Convention => C, External_Name => "mylib__identity";

end Mylib;
