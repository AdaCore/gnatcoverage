pragma Ada_2022;
pragma Assertion_Policy (Check);

package Lib is
  function Id (B : Boolean) return Boolean is (B)
    with Pre  => B or else not B or else B,
         Post => Id'Result = B;
end Lib;
