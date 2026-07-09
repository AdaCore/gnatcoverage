pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2 : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Full_Decision,"J1");
      if C1 and then C2 then  -- # condition
         Put_Line (Message);  -- # put_line
      end if;
   end Print_If;
end Pkg;
