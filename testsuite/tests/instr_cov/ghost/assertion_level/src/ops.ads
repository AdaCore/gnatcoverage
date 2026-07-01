pragma Ada_2012;

package Ops is

   function Has_Next_Impl (X : Integer) return Boolean
   is (X < Integer'Last)                  -- # ghost-code
   with Ghost => Static;

   function Has_Next (X : Integer) return Boolean
   is (Has_Next_Impl (X))                 -- # ghost-code
   with Ghost => Static;

   procedure Bump (X : in out Integer)
   with Pre => (Static => Has_Next (X));  -- # ghost-assert

end;
