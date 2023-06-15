package body Pak is

   procedure Callback is null;  -- # npp

   procedure Call is
   begin
      P2 (Callback'Access); -- # npc
   end Call;

end Pak;
