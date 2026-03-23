package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      loop
         exit when A or else B;  -- # evalStmt :o/d:
         return False;           -- # decisionFalse
      end loop;
      return True;               -- # decisionTrue
   end;
end;
