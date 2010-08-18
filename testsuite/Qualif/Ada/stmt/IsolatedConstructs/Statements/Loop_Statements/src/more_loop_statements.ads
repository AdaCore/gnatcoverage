--  Contains routines with different forms of LOOP statements in different
--  kinds of program units. All the  routines in this package use loop
--  statements with no control statements that transfer control out of the
--  loop.

with LOOP_Statements_Support; use LOOP_Statements_Support;
package More_LOOP_Statements is
   pragma Elaborate_Body;

   Global_String : String (1 .. 10);
   Global_Sample : Sample (1 .. 10);

   function N_Of (L : Level; S : Sample) return Natural;
   --  Count and return the number of values equal to L in sample S. The
   --  implementation uses WHILE loop

   procedure Change_Char
     (S      : in out String;
      Old_Ch :        Character;
      New_Ch :        Character);
   --  Replaces in S Old_Ch with New_Ch. The implementation uses FOR loop

end More_LOOP_Statements;

