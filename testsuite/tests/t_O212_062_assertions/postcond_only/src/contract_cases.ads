pragma Assertion_Policy (Check);
pragma Ada_2022;

package Contract_Cases is
    function F (I : Integer) return Integer
       with Contract_Cases =>
         ((I in 0 .. 10) =>                     -- # guard-1
            F'Result in 0 .. 5,                 -- # conseq-1
          others         =>
            F'Result not in 0 .. 5);            -- # conseq-2
end Contract_Cases;
