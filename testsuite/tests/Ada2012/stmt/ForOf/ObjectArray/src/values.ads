
package Values is

   type Object is tagged record
      X : Integer;
   end record;

   type Array_Type is array (1 .. 8) of Object;

   procedure Touch (E : in out Object);

end;
