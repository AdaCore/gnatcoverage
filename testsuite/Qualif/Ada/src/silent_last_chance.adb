package body Silent_Last_Chance is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      procedure C_exit (Status : Integer);
      pragma Import (C, C_exit, "_exit");
      pragma No_Return (C_exit);
   begin
      --  No return procedure.
      C_exit (0);
   end Last_Chance_Handler;

end;
