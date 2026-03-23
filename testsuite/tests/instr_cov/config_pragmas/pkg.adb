with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   protected body PT is

      ----------
      -- Init --
      ----------

      procedure Init (Cond : Boolean) is
      begin
         PT.Cond := Cond;
      end Init;

      ---------------
      -- Wait_Cond --
      ---------------

      entry Wait_Cond
         when Cond
      is
      begin
         Put_Line ("OK");
      end Wait_Cond;

   end PT;
end Pkg;
