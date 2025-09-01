with Assert;
with Commands; use Commands;

procedure Test_Cmd_Unsafe is
begin
   --  Stepping forward without room ahead is always unsafe
   Assert (not Safe (Cmd => Step, Front => Rock));
   Assert (not Safe (Cmd => Step, Front => Pit));
end Test_Cmd_Unsafe;
