package body PandPor is

   function F (A, B, C : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;  -- # decl
      Touched : Bmap := (others => False);  -- # decl
   begin
      for Value in False .. ((A and then B) or else C) loop -- # evalStmt :o/e:
         Touched (Value) := True;                          -- # returnValue
      end loop;
      return Touched (True);  -- # returnValue
   end;
end;
