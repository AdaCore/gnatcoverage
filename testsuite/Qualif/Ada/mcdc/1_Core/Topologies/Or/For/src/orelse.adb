package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;  -- # decl
      Touched : Bmap := (others => False);  -- # decl
   begin
      for Value in False .. (A or else B) loop -- # evalStmt :o/e:
         Touched (Value) := True;             -- # returnValue
      end loop;
      return Touched (True);  -- # returnValue
   end;
end;
