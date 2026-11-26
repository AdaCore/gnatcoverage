pragma Assertion_Policy (Check);
pragma Ada_2022;

package Contract_Cases is
    function F (I : Integer) return Integer
       with Contract_Cases =>
         ((I in 0 .. 10) =>                     -- # guard-1
            F'Result in 0 .. 5,                 -- # conseq-1
          others         =>
            F'Result not in 0 .. 5);            -- # conseq-2

   --  The guard part of single-clause Contract_Cases aspects must be
   --  considered as a precondition (it used to be considered as a
   --  postcondition with --postcond-only).

   function F2 (I : Integer) return Integer
      with Contract_Cases =>
        ((I in Integer'First .. Integer'Last) => -- # f2-guard
           F2'Result in 0 .. 1);                 -- # f2-conseq
end Contract_Cases;
