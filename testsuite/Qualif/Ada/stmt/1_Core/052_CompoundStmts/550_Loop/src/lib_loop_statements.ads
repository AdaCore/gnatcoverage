-- Package featuring a loop controlled by a decision within
-- its body elaboration code.

package Lib_Loop_Statements is
   Values : array (1 .. 8) of Integer := (others => 0);
   function N_Ones return Integer;
end;
