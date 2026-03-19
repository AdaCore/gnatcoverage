package body State is
   procedure Tick is
   begin
      Ticks := Ticks + 1;
   end;
   procedure Dtick is
   begin
      Dticks := Dticks + 1;
   end;
end;
