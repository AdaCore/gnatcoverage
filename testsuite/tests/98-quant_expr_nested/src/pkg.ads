pragma Ada_2012;

package Pkg is

   type My_Arr is array (Positive range <>) of Integer;

   function All_Positive
     (Arr_Main, Arr_Backup : My_Arr; Use_Backup : Boolean) return Boolean is
     (for all Element of  -- # stmt
        My_Arr'(if Use_Backup then Arr_Backup else Arr_Main) =>  -- # if_expr
        Element >= 0);  -- # predicate

end Pkg;
