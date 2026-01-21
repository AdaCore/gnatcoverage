pragma Ada_2012;

package body Pak is

   procedure Call is
      X : aliased constant R := (null record);  -- # npc decl
   begin
      PR5 (X'Access); -- # npc
   end Call;

end Pak;
