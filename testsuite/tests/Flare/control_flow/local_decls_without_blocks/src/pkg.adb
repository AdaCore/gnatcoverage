pragma Flare_0_1;
with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   function Square (I : Integer) return Integer is
   begin
      if I = 1 then                       -- # if
         return 1;                        -- # early
      end if;

      Result : constant Integer := I * I; -- # local_decl
      return Result;                      -- # return
   end Square;
end Pkg;
