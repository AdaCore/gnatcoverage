------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Commands; use Support, Commands;

procedure Test_Cmd_Safe is
begin
   --  Remaining still is always safe, as well as stepping
   --  forward with room ahead

   Assert (Safe (Cmd => Hold, Front => Rock));
   Assert (Safe (Cmd => Hold, Front => Pit));
   Assert (Safe (Cmd => Step, Front => Room));
end Test_Cmd_Safe;
