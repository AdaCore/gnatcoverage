with Support, Starts; use Support, Starts;

procedure Test_Starts_T is
   S : aliased String := "Hello";
begin
   Assert (Starts3 (S'Unchecked_Access, 'H') = True);
end;

--# starts.adb
-- /null/     l! dF-
-- /contents/ l! 0
