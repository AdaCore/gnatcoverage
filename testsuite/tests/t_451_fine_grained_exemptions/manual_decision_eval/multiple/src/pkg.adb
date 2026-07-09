pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2, C3 : Boolean; Message : String) is
   begin
      if C1 and then C2 and then C3 then  -- # condition_1
         Put_Line (Message);              -- # put_line
      end if;

      if C1 and then C2 then              -- # condition_2
         Put_Line (Message);              -- # put_line
      end if;
   end Print_If;
end Pkg;
