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
      pragma Precondition
        (Identity (R1.Valid and then R2.Valid));   -- # preValid
   begin
      return Identity (R2.X <= R1.Y)      -- # retStmt
        and then Identity (R2.Y >= R1.X); -- # retLine
   end;

end;
