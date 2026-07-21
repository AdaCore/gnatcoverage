pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2, C3 : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Full_Decision, 1, "J");
      Put_Line                                                 -- # put_line_0
        (if C1                                                 -- # condition_0
         then (if C2 and then C3 then Message else "none-c2")  -- # condition_1
         else "none-c1");                                      -- # put_line_x
   end Print_If;
end Pkg;
