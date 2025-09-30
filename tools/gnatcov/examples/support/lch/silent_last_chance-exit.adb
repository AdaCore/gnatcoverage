package body Silent_Last_Chance is

   --  Use exit(0) to terminate silently with a status code
   --  indicating success and run atexit handlers, crucial to
   --  get meaningful traces out of instrumented programs in native
   --  configurations.

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      procedure C_exit (Status : Integer);
      pragma Import (C, C_exit, "exit");
      pragma No_Return (C_exit);

      procedure Lch_Enter;
      pragma Import (Ada, Lch_Enter, "__lch_enter");
   begin
      Lch_Enter;
      C_exit (0);
   end Last_Chance_Handler;

end;
