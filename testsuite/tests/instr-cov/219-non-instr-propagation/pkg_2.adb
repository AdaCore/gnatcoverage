with Ada.Text_IO; use Ada.Text_IO;

package body Pkg_2 is
   protected body PT is

      --------------
      -- Set_Cond --
      --------------

      procedure Set_Cond (Value : Boolean) is
      begin
         if Value then       --  SCO 3+4+5 (stmt+decision+condition)
            Put_Line ("OK"); --  SCO 6 (stmt)
         end if;
         Cond := True;       --  SCO 7 (stmt)
      end Set_Cond;

      ---------------
      -- Wait_Cond --
      ---------------

      entry Wait_Cond when Cond is --  SCO 8+9 (decision+condition)
      begin
         Put_Line ("OK");          --  SCO 10 (stmt)
      end Wait_Cond;

   end PT;
end Pkg_2;
