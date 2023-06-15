package body Pak is

   procedure PPR2 (X : access PR) is null; -- # npb

   procedure Call is
      X : aliased PR;  -- # npc decl
   begin
      PPR2 (X'Access); -- # npc
   end Call;

end Pak;
