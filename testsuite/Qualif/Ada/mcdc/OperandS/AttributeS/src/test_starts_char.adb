with Support, Starts; use Support, Starts;

procedure Test_Starts_Char is
   Hello : aliased String := "Hello";
begin
   Assert (Starts3 (Hello'Unchecked_Access, 'H') = True);
   Assert (Starts3 (Hello'Unchecked_Access, 'X') = False);
end;

--# starts.adb
-- /null/     l! c!:"S./=.null"
-- /contents/ l! c!:"S'Length"
