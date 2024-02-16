
package Values is

   type Object is tagged record
      A, B, Expr : Boolean;
   end record;

   type Array_Type is array (Natural range <>) of Object;

   procedure Process (A : in out Array_Type);
end;
