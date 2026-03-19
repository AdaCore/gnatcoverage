with Ada.Text_IO; use Ada.Text_IO;

package body Pkg_1 is
   protected body PT is

      procedure Say_Hello is
      begin
         Put_Line ("Hello"); --  SCO 3 (stmt)
      end Say_Hello;

      ---------------
      -- Wait_Cond --
      ---------------

      entry Wait_Cond when Cond is --  SCO 4+5 (decision+condition)
      begin
         Put_Line ("OK");          --  SCO 6 (stmt)
      end Wait_Cond;

      --------------
      -- Set_Cond --
      --------------

      procedure Set_Cond (Value : Boolean) is
      begin
         if Value then       --  SCO 7-8+9 (stmt+decision+condition)
            Put_Line ("OK"); --  SCO 10 (stmt)
         end if;
         Cond := True;       --  SCO 11 (stmt)
      end Set_Cond;

   end PT;
end Pkg_1;
