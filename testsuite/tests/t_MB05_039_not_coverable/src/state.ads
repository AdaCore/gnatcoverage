package State is

   -- Liveness ticks, expected to click

   Ticks : Integer := 0;
   pragma Volatile (Ticks);

   -- Death ticks, expected never to clisk

   Dticks : Integer := 0;
   pragma Volatile (Dticks);

   procedure Tick;
   procedure Dtick;
end;
