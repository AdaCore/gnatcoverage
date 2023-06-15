
package Values is
   
   type Object is tagged record
      X : Integer;
      Count : Natural;
   end record;
   
   type Array_Type is array (Natural range <>) of Object;
   
   procedure Process (A : in out Array_Type; Bump : Boolean);
end;
