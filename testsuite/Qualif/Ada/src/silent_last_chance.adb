package body Silent_Last_Chance is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      procedure C_abort;
      pragma Import (C, C_abort, "abort");
      pragma No_Return (C_abort);
   begin
      --  No return procedure.
      C_abort;
   end Last_Chance_Handler;

end;
