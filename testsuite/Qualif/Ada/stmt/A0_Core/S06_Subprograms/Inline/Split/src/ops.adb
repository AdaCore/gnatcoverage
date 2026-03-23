package body Ops is

   pragma Suppress (All_Checks);

   procedure Bump (X : in out Integer; Up : Boolean);
   pragma Inline (Bump);

   procedure Bump (X : in out Integer; Up : Boolean) is
   begin
      if Up then      -- # eval
         X := X + 1;  -- # up
      else
         X := X - 1;  -- # down
      end if;
   end;

   procedure Up (X : in out Integer) is
   begin
      Bump (X, Up => True); -- # up
   end;

   procedure Down (X : in out Integer) is
   begin
      Bump (X, Up => False); -- # down
   end;

end;
