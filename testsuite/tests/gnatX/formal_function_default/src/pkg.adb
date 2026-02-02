pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   package body Grid is

      -----------------
      -- Point_Image --
      -----------------

      function Point_Image (Self : Point) return Character is
      begin
         if Is_Center (Self) then
            return '+';
         else
            return '.';
         end if;
      end Point_Image;

      -----------
      -- Print --
      -----------

      procedure Print is
      begin
         for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
               Put (Point_Image ((X, Y)));
            end loop;
            New_Line;
         end loop;
      end Print;

   end Grid;

end Pkg;
