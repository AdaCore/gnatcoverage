package body Pak is

   procedure Call is
      X : aliased R;  -- # npc decl
   begin
      PR3 (X'Access); -- # npc
   end Call;

end Pak;
