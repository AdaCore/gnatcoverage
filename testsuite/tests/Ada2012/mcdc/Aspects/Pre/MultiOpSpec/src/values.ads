pragma Ada_2012;
pragma Assertion_Policy (Pre => Check);

package Values is

   type Int is record
      Value : Integer;
      Valid : Boolean;
   end record;

   function Plus (A, B : Int) return Integer
     with Pre => (A.Valid and then B.Valid); -- # eval

end;
