package body AndIdOr is
   function F (A, B, C : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;  -- # decl
      Touched : Bmap := (others => False);  -- # decl
   begin
      for V in False .. (A and then Identity (B or else C)) loop -- # evalStmt :o/e:
         Touched (V) := True;         -- # returnValue
      end loop;
      return Touched (True);  -- # returnValue
   end;
end;
