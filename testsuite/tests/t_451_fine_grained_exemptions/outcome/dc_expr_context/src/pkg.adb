with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2 : Boolean; Message : String) is
      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "always true");
      Condition : constant Boolean := C1 and then C2;  -- # expr_cond
   begin
      if Condition then                                -- # if_cond
         Put_Line (Message);                           -- # put_line
      end if;
   end Print_If;
end Pkg;
