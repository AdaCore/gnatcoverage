pragma Ada_2012;

package body Pak is

   function Callback (S : access String) return Natural
   is (S.all'Length);  -- # npp

   procedure Call is
   begin
      P2 (Callback'Access); -- # npc
   end Call;

end Pak;
