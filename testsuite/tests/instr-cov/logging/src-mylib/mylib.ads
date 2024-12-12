package Mylib is

   function Next (I : Integer) return Integer;

   function Identity (I : Integer) return Integer
   with Import, Convention => C, External_Name => "mylib__identity";

end Mylib;
