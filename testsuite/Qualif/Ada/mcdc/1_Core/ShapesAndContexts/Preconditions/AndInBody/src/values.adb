pragma Check_Policy (Precondition, On);

package body Values is

   procedure Set (V : out Int; Val : Integer) is
   begin
      V := (Set => True, Val => Val); -- # set
   end;

   function Value (V : Int) return Integer is
   begin
      return V.Val; -- # value
   end;

   function Same (V1, V2 : Int) return Boolean is
      --  2 operands decision in body precondition here
      pragma Precondition (V1.Set and then V2.Set); -- # pre
   begin
      return Value (V1) = Value (V2); -- # same
   end;
end;

