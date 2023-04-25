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
   begin
      pragma Annotate                                    -- # preValid
        (Xcov, Exempt_On, "expect no invalid ranges");   -- # preValid
      if not (R1.Valid and then R2.Valid) then  -- # preValid_if
         raise Program_Error;                   -- # preValid_raise
      end if;                                   -- # preValid
      pragma Annotate (Xcov, Exempt_Off);                -- # preValid

      if R2.X <= R1.Y and then R2.Y >= R1.X then -- # checkOverlap
         return True;  -- # overlapTrue
      else
         return False; -- # overlapFalse
      end if;
   end;

end;
