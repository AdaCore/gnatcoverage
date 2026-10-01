pragma Ada_2012;
pragma Assertion_Policy (Check);

package Pkg is
   procedure P (A, B : Boolean) is null  -- # stmt
   with Contract_Cases =>
     (A or else B =>                     -- # guard-1
        A or else B,                     -- # conseq-1
      others      =>                     -- # guard-2
        not A and then not B);           -- # conseq-2
end Pkg;
