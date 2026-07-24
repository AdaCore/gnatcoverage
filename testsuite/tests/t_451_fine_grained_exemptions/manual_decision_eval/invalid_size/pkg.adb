pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2 : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Manual_Decision_Evaluation, True, "J1");
      pragma Annotate
        (Xcov, Manual_Decision_Evaluation, True, True, True, "J2");
      if C1 and then C2 then
         Put_Line (Message);
      end if;
   end Print_If;
end Pkg;
