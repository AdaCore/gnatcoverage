with Support, Starts; use Support, Starts;

procedure Test_Starts_F is
   Hello : aliased String := "Hello";
begin
   Assert (Starts3 (null, 'H') = False);
   Assert (Starts3 (Hello'Unchecked_Access, 'G') = False);
end;

--# starts.adb
-- /null/     l! dT-
-- /contents/ l! 0
