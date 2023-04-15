package body Silent_Last_Chance is

   --  Use abort() to terminate silently and expect this won't
   --  trigger an error exit code out of whatever executes the
   --  code.

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);

      procedure C_Abort;
      pragma Import (C, C_abort, "abort");
      pragma No_Return (C_abort);

      procedure Lch_Enter;
      pragma Import (Ada, Lch_Enter, "__lch_enter");
   begin
      Lch_Enter;
      C_abort;
   end Last_Chance_Handler;

end;
