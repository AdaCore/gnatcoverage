
package Values is
   
   type Object is tagged record
      X : Integer;
   end record;
   
   procedure Touch (E : in out Object);
   procedure Check_Value (E : Object; V : Integer);
 
   type Array_Type is array (1 .. 8) of Object;
   VA : Array_Type := (others => (X => 5));
end;
