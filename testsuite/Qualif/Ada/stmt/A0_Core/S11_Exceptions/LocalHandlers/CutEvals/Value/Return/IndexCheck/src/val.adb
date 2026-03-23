with Args; use Args;

package body Val is

   pragma Unsuppress (All_Checks);

   function To_Bool (A : Integer) return Boolean is
   begin
      -- Possible index check failure here
      return Bool_For (A); -- # eval
   end;

   procedure Bool (A : Integer; R : out Boolean) is
   begin
      R := To_Bool (A); -- # eval
   end;

end;
