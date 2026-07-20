pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2 : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "never false");
      Put_Line ("Maybe about to print a message...");

      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, 2, "never false");
      Put_Line
        (if C1
         then (if C2 then Message else "none-c2")
         else "none-c1");

      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "never false");
   end Print_If;
end Pkg;
