pragma Ada_2022;

package body Pkg is

   function Identity (A : Boolean; Do_Raise : Boolean) return Boolean is
   begin
      return                                                  -- # stmt
        (not Do_Raise or else raise Custom_Error) and then A; -- # expr :o/e:
   end Identity;

end Pkg;
