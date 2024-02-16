package Values is
   X : Integer := 1;

   function One return Float;

   procedure Latch (V : Integer);
   function Value_Of (X : Integer) return Integer;
   function Value_Of_X return Integer;
end;
