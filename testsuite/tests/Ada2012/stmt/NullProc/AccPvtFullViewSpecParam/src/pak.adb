package body Pak is

   procedure Call is
      X : aliased PR;  -- # npc decl
   begin
      PPR1 (X'Access); -- # npc
   end Call;

end Pak;
