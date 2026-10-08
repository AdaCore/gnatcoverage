with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   package body Gen is

      procedure Check (Self : Integer) is
      begin
         if Predicate (Self) then
            Put_Line ("Predicate satisfied");
         end if;
      end Check;

   end Gen;

end Pkg;
