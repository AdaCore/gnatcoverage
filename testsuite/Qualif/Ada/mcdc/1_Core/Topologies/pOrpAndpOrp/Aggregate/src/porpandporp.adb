package body PorPandPorP is

   type Expr is record
      Value : Boolean;
   end record;

   function F (A, B, C, D : Boolean) return Boolean is
      E : Expr := -- # returnValue
        (Value => (A or else B) and then (C or else D));  -- # evalOther
   begin
      return E.Value;  -- # returnValue
   end;

end;
