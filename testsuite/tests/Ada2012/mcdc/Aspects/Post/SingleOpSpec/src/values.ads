pragma Ada_2012;
pragma Assertion_Policy (Post => Check);

package Values is
   
   function Plus (A, B : Integer) return Integer
     with Post => (Plus'Result > 0); -- # eval
   
end;
