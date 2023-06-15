package body Pak is

   procedure Call is
      X : aliased Integer; -- # npc decl
   begin
      P3 (X'Access); -- # npc
   end Call;

end Pak;
