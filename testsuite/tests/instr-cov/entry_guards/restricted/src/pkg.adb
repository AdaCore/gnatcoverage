with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   protected body PT is

      ----------
      -- Init --
      ----------

      procedure Init (A, B : Boolean) is
      begin
         PT.A := A;
         PT.B := B;
      end Init;

      ---------------
      -- Wait_Cond --
      ---------------

      entry Wait_Cond
         when A and then B
      is
      begin
         Put_Line ("OK");
      end Wait_Cond;

   end PT;
end Pkg;
