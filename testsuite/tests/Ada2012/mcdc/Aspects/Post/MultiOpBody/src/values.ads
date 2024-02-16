
package Values is

   type Int is record
      Value : Integer;
      Valid : Boolean;
   end record;

   procedure Filter (A, B : in out Int);

end;
