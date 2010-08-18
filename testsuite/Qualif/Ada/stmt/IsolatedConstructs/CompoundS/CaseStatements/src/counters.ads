package Counters is
   type OpKind is (Inc, Dec);
   procedure Step_By (N : Natural; X : in out Integer; Op : OpKind);
   --  Add/Substract N to/from X when Op is Inc/Dec. Use a case statement
   --  to operate one way or the other.
end;
