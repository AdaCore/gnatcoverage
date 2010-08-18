with Support, Starts; use Support, Starts;

procedure Test_Starts_Len is
   Hello : aliased String := "Hello";
   Empty : aliased String := "";
begin
   Assert (Starts3 (Hello'Unchecked_Access, 'H') = True);
   Assert (Starts3 (Empty'Unchecked_Access, 'H') = False);
end;

--# starts.adb
-- /null/     l! c!:"S /= null"
-- /contents/ l! c!:"S.S'First."
