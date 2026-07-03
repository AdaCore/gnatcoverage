pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2 : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, "never false");
      pragma Annotate (Xcov, Exempt_Decision_Outcome, True, "never true");
      Put_Line                                    -- # put_line_0
        (case C1 is                               -- # put_line_x
         when False => "none-c1",                 -- # put_line_x
         when True  =>                            -- # put_line_x
           (if C2 then Message else "none-c2"));  -- # condition
   end Print_If;
end Pkg;
