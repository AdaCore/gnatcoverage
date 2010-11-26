with Support, Starts; use Support, Starts;

procedure Test_Starts_Null is
   Hello : aliased String := "Hello";
begin
   Assert (Starts3 (Hello'Unchecked_Access, 'H') = True);
   Assert (Starts3 (null, 'H') = False);
end;

--# starts.adb
-- /null/     l! 0
-- /contents/ l! m!:"S'Length",m!:"S.S'First."
