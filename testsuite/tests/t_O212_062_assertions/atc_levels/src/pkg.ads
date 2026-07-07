pragma Assertion_Level (Level_1);
pragma Assertion_Level (Level_2);
pragma Assertion_Policy (Check);
pragma Ada_2022;

package Pkg is

   function F (X, Y : Integer) return Boolean
   is (X in Integer)      -- # body
   with Pre =>
     (Level_1 => X > 0,   -- # pre1
      Level_2 => Y > 0);  -- # pre2

end Pkg;
