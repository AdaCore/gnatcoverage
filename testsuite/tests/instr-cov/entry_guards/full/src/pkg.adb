with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is
   protected body PT is

      ----------
      -- Init --
      ----------

      procedure Init (A, B : Boolean) is
      begin
         PT.A := A;  -- # init
         PT.B := B;  -- # init
      end Init;

      ---------------
      -- Wait_Cond --
      ---------------

      entry Wait_Cond
         when A and then B  -- # guard
      is
      begin
         Put_Line ("OK");  -- # stmt
      end Wait_Cond;

   end PT;
end Pkg;
