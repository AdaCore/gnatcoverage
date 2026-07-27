with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   procedure Process (C : Character) is
   begin
      case C is                                           -- # case
         when ASCII.NUL =>
            Put_Line ("NUL");                             -- # when_nul
         when 'A' .. 'Z' =>
            Put_Line ("Uppercase letter");                -- # when_upper
         when 'a' .. 'z' =>
            pragma Annotate (Xcov, Exempt_Branch, "J1");  -- # when_lower
            Put_Line ("Lowercase letter");                -- # when_lower_s
         when '0' .. '9' =>
            Put_Line ("Digit");                           -- # when_digit
         when others =>
            pragma Annotate (Xcov, Exempt_Branch, "J2");  -- # when_other
            Put_Line ("Other");                           -- # when_other_s
      end case;
   end Process;
end Pkg;
