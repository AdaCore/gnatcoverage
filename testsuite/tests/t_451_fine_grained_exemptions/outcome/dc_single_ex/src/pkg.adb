with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (Condition : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "always true");
      if Condition then       -- # condition
         Put_Line (Message);  -- # put_line
      end if;
   end Print_If;
end Pkg;
