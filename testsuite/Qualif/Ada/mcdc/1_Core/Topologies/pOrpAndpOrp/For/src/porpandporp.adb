package body POrPAndPorP is

   function F (A, B, C, D : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;  -- # decl
      Touched : Bmap := (others => False);  -- # decl
   begin
      for Value in  -- # returnValue
        False .. ((A or else B) and then (C or else D))  -- # evalOther :o/e:
      loop
         Touched (Value) := True;  -- # returnValue
      end loop;
      return Touched (True);       -- # returnValue
   end;
end;

