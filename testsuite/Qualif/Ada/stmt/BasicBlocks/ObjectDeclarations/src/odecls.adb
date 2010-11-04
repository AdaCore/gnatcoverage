
package body Odecls is
   procedure Check_Locals is
      My_X_Noinit : Integer;                         -- # stmt
      My_X_Init : Integer := Support.Identity (12);  -- # stmt
      My_Msg : Msg (Len => 12);                      -- # stmt
   begin
      My_X_Noinit := My_X_Init;       -- # stmt
      Latch_Int := My_X_Init;         -- # stmt
      Support.Assert (My_Msg.Valid);  -- # stmt
   end;
end;
