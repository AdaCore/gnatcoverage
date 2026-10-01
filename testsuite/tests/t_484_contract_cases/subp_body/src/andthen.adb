pragma Ada_2012;
pragma Assertion_Policy (Check);

package body Andthen is

   function Helper (Left, Right : Boolean) return Boolean
      with Contract_Cases =>
        (Left or else Right =>                     -- # guard-1
           Helper'Result = (Left and then Right),  -- # conseq-1
         others             =>                     -- # guard-2
           not Helper'Result)                      -- # conseq-2
   is
   begin
      return Left and then Right;                  -- # stmt
   end Helper;

   function Andthen (Left, Right : Boolean) return Boolean is
   begin
      return Helper (Left, Right);                 -- # stmt
   end Andthen;
end Andthen;
