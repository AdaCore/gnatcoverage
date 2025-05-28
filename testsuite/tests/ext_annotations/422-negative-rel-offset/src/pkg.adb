with Ada.Text_IO;

package body Pkg is

   procedure Check_Ok is
      Val : Boolean := False;  -- # ok_st
   begin
      if Val then              -- # ex_dc
         raise Program_Error;  -- # ex_st
      else
         Ada.Text_IO.Put_Line ("All ok");  -- # ok_st
      end if;
   end Check_Ok;

end Pkg;
