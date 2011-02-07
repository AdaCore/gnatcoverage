--  Contains routines with GOTO statements that transfer control out of LOOP
--  statements

package GOTO_Statements is

   type Sample is array (Natural range <>) of Integer;

   --  All the three procedures below do the same processing on the argument
   --  Arr (processing is entirely meaningless, see the bodies for details,
   --  the main goal is to put GOTO statements in some typical context). The
   --  only difference between procedures is the form of a LOOP statement.

   procedure Update_Sample_For
     (Arg  : in out Sample;
      Par1 : in out Integer;
      Par2 : in out Integer);
   --  Uses FOR loop

   procedure Update_Sample_While
     (Arg  : in out Sample;
      Par1 : in out Integer;
      Par2 : in out Integer);
   --  Uses WHILE loop

   procedure Update_Sample
     (Arg  : in out Sample;
      Par1 : in out Integer;
      Par2 : in out Integer);
   --  Uses unconditional loop

end GOTO_Statements;
