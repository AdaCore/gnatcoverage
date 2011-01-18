pragma Check_Policy (Precondition, On);

package body Values is
   procedure Set (V : in out Int; Val : Integer; Reset_OK : Boolean) is
   begin
      V := (Set => True, Val => Val); -- # set
   end;
end;
