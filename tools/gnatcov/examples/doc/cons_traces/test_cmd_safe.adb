with Assert;
with Commands; use Commands;

procedure Test_Cmd_Safe is
begin
   --  Remaining still is always safe, as is stepping with room ahead:
   Assert (Safe (Cmd => Hold, Front => Rock));
   Assert (Safe (Cmd => Hold, Front => Pit));
   Assert (Safe (Cmd => Step, Front => Ground));
end Test_Cmd_Safe;
