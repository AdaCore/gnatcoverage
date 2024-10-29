with Interfaces.C; use Interfaces.C;

package Pkg is

   function Do_Math (L, R : int) return int;
   pragma Export (C, Do_Math, External_Name => "do_math");

end Pkg;
