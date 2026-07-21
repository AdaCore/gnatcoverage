pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2 : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Decision_Condition, 1, "J1");
      Put_Line ("Maybe about to print a message...");

      pragma Annotate (Xcov, Exempt_Decision_Condition, 3, "J2");
      Put_Line (if C1 and then C2 then Message else "none");
   end Print_If;
end Pkg;
