pragma Ada_2012;
pragma Assertion_Policy (Post => Check);

package body Values is
   
   procedure validate (A, B : in out Int)
      with Post => (A.Valid and then B.Valid) -- # eval
   is
   begin
      A.Valid := A.Value > 0; -- # stmt
      B.Valid := B.Value > 0; -- # stmt
   end;
   
   procedure Filter (A, B : in out Int) is
   begin
      Validate (A, B); -- # stmt
   end;

end;
