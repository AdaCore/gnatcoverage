
procedure P is

   procedure Assert (T : Boolean) is
   begin
      pragma Annotate (Xcov, Exempt_On, "assert expected never to trip");
      if not T then            -- # expect-never-true
         raise Program_Error;  -- # expect-uncovered
      end if;
      pragma Annotate (Xcov, Exempt_Off, "assert expected never to trip");
   end;

   procedure Inc (X : in out Integer) is
   begin
      X := X + 1;
   end;

   V : Integer := 12;
   pragma Volatile (V);

begin
   Inc (V);
   Assert (V = 13);
end;
