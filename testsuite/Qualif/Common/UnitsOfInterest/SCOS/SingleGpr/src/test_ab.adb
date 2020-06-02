with Support; use Support;
with Monitor;

procedure Test_AB is
begin
   Assert (Monitor.Hit = 0);
   Assert (Monitor.Miss = 0);

   Monitor.Diamond (True, True, False);

   Assert (Monitor.Hit = 1);
   Assert (Monitor.Miss = 0);

   Monitor.Diamond (False, True, False);

   Assert (Monitor.Hit = 1);
   Assert (Monitor.Miss = 1);

   Monitor.Diamond (False, False, False);

   Assert (Monitor.Hit = 1);
   Assert (Monitor.Miss = 2);
end;
