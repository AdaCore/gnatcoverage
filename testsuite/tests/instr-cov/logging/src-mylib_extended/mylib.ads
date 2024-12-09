package Mylib is
   function Next (I : Integer) return Integer;
   function Prev (I : Integer) return Integer is (I - 1);
end Mylib;
