pragma Ada_2022;

package body Pkg is

   function Identity (A : Boolean; Do_Raise : Boolean) return Boolean is
   begin
      if                                                     -- # stmt
        (not Do_Raise or else raise Custom_Error) and then A -- # expr :o/d:
      then
         return True;  -- # ret_true
      else
         return False;  -- # ret_false
      end if;
   end Identity;

end Pkg;
