package body Notornot is

   function F (A, B : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;  -- # decl
      Touched : Bmap := (others => False);  -- # decl
   begin
      for Value in False .. ((not A) or else (not B)) loop -- # evalStmt
         Touched (Value) := True;             -- # returnValue
      end loop;
      return Touched (True);  -- # returnValue
   end;
end;
