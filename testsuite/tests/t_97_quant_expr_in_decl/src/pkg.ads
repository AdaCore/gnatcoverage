package Pkg is

   type My_Arr is Array (Positive range <>) of Boolean;

   function All_True (Arr : My_Arr) return Boolean;

   function Some_True (Arr : My_Arr) return Boolean;

end Pkg;
