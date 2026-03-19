pragma Ada_2012;

with Common;

package body Ops is
   pragma Suppress(All_Checks);

   function Bumpable (X : Integer) return Boolean
     with Ghost
   is
   begin
      -- Decision on purpose here, just to strengthen the check

      if X < Integer'last then -- # ghost-if
         return True;          -- # ghost-then
      else
         return False;         -- # ghost-else
      end if;
   end;

   --  The code below only invokes ghost entities but is not ghost
   --  itself. We still don't expect violations on the ghost pieces
   --  when this is not called.

   --  Beware not to induce such violations by back-propagation.

   procedure Check_Bumpable (X : Integer) is
   begin
      N_Checks := N_checks + 1;    -- # nchecks
      Ghost_Check := Bumpable (X); -- # ghost-call
   end;
end;
