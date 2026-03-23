package Pkg is

   function To_Pos (X : Integer; Do_Raise : Boolean) return Integer;
   --  Return the absolute value of X. If Do_Raise is True, and exception is
   --  raised in the main decision.

end Pkg;
