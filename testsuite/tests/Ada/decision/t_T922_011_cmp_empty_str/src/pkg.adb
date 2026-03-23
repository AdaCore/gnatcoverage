with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Dump (E : Enum) is
      Img : constant String := Pkg.Enum_Images (E).all;  -- # decl
   begin
      if Img /= "" then                                  -- # cond
         Put_Line ("Image: " & Img);                     -- # then
      end if;
   end Dump;

end Pkg;
