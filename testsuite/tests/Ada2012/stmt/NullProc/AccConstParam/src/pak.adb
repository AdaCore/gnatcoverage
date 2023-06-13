package body Pak is

   procedure Call is
      X : aliased constant Integer := 1;  -- # npc decl
   begin
      P4 (X'Access); -- # npc
   end Call;

end Pak;
