package body Notornot is
   type My_Type (Value : Boolean) is null record;

   function F (A, B : Boolean) return Boolean is
      R : My_Type (Value => (not A) or else (not B));  -- # evalStmt :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
