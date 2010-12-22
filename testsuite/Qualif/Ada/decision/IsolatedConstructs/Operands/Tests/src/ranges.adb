pragma Check_Policy (Precondition, On);

package body Ranges is

   procedure Set (R : out XYrange; X, Y : Integer) is
   begin
      if Y < X then                             -- # checkValid
         R := (Valid => False, X => 0, Y => 0); -- # assignInvalid
      else
         R := (Valid => True, X => X, Y => Y);  -- # assignValid
      end if;
   end;

   function Overlap (R1, R2 : XYrange) return Boolean is
      --  No test in condition here. Useful check to make
      --  sure the callers abide expectations
      pragma Precondition (R1.Valid and then R2.Valid));  -- # preValid
   begin
      --  Conditions with tests here:
      return R2.X <= R1.Y and then R2.Y >= R1.X; -- # evalOverlap
   end;

end;
