pragma Ada_2022;

package body Pkg is

   function To_Pos (X : Integer; Do_Raise : Boolean) return Integer is
   begin
      if (Do_Raise and then raise Program_Error) or else X >= 0 then -- # eval
         return X;                                                   -- # true
      else
         return -X;                                                  -- # false
      end if;
   end To_Pos;

end Pkg;
