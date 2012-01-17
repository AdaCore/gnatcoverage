------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      procedure C_Abort;
      pragma Import (C, C_Abort, "abort");
      pragma No_Return (C_Abort);
   begin
      Put_Line ("!!!!!!!!!!!!!!!!!!!!!!");
      Put_Line ("!! EXCEPTION RAISED !!");
      Put_Line ("!!!!!!!!!!!!!!!!!!!!!!");

      C_Abort;
   end Last_Chance_Handler;

   procedure Assert (T : Boolean) is
   begin
      if not T then
         raise Program_Error;
      end if;
   end Assert;
end Support;
