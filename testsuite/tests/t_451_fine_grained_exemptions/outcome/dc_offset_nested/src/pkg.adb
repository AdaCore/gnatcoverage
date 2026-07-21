pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Print_If (C1, C2 : Boolean; Message : String) is
   begin
      pragma Annotate (Xcov, Exempt_Decision_Outcome, False, 2, "never false");
      Put_Line                                       -- # put_line_0
        (declare                                     -- # put_line_x
           D1 : constant Boolean := C1 and then C2;  -- # put_line_d
           D2 : constant Boolean := C1 or else C2;   -- # put_line_d
         begin                                       -- # put_line_x
           (if D1 and then D2                        -- # condition
            then Message                             -- # put_line_x
            else ""));                               -- # put_line_x
   end Print_If;
end Pkg;
