--  Contains declarations needed for LOOP statement tests

package LOOP_Statements_Support is

   type Level is range 0 .. 255;
   type Sample is array (Natural range <>) of Level;

   type Big_Sample is array (Natural range <>) of Integer;

end LOOP_Statements_Support;

