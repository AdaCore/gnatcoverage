pragma Ada_2012;

package body Ops is
   pragma Suppress(All_Checks);

   --  A local decl would get a violation through back propagation,
   --  which looks suspicious as we presumably should prevent the
   --  production of a SCO at all for such entities. ???

   procedure Monitor (A, B : Boolean) is
      Helper : Boolean := A and then B with Ghost; -- # ghost
   begin
      Ops.A := A; -- # nchecks
      Ops.B := B; -- # nchecks
      N_Checks := N_Checks + 1; -- # nchecks
   end;
end;
