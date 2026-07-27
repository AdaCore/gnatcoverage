package body Pkg is

   procedure Check (X : Integer; Ok : out Boolean) is
   begin
      Ok := False;
      pragma Annotate (Xcov, Exempt_On, "defensive code");
      if X < 0 then
         Ok := False;
         return;
      end if;
      pragma Annotate (Xcov, Exempt_Off);
      Ok := X > 10;
   end Check;

   procedure Skipped (X : Integer; Y : out Integer) is
   begin
      pragma Annotate (Xcov, Cov_Off, "not tested yet");
      Y := X + 1;
      Y := Y * 2;
      pragma Annotate (Xcov, Cov_On);
   end Skipped;

end Pkg;
