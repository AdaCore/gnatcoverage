package body Last_Chance_Handler is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      procedure C_Abort;
      pragma Import (C, C_Abort, "abort");
      pragma No_Return (C_Abort);
   begin
      --  No return procedure.
      C_Abort;
   end Last_Chance_Handler;

end Last_Chance_Handler;
