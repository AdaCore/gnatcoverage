package body Silent_Last_Chance is

   --  Use exit(0) to terminate silently with a status code
   --  indicating success. Abort would result in an error status
   --  and _exit would bypass the atexit handlers, crucial to
   --  get meaningful traces out of instrumented programs in native
   --  configurations.

   --  This requires dragging libc explicitly in BB configurations.
   pragma Linker_Options ("-lc");

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      procedure C_exit (Status : Integer);
      pragma Import (C, C_exit, "exit");
      pragma No_Return (C_exit);
   begin
      C_exit(0);
   end Last_Chance_Handler;

end;
