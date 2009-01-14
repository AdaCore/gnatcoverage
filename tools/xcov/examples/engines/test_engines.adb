with Last_Chance_Handler;
with Engines; use Engines;

------------------
-- Test_Engines --
------------------
procedure Test_Engines is
   E : Engine;
begin

   --  Exercise various conditions and decisions in State_Of.  Incomplete, as
   --  this doesn't check the behavior on equality wrt thresholds, and misses
   --  the Alarming combinations.

   E.T := Stable_T + 1;
   E.P := Stable_P + 1;
   if State_Of (E) /= Critical then
      raise Program_Error;
   end if;

   E.T := Stable_T - 1;
   E.P := Stable_P - 1;
   if State_Of (E) /= Stable then
      raise Program_Error;
   end if;

end;
