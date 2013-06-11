with System;

package Silent_Last_Chance is

   Unexpected_Last_Chance_Call : Integer := 0;
   pragma Export (Ada, Unexpected_Last_Chance_Call,
                  "__gnat_unexpected_last_chance_call");
   --  This declaration and comment should appear in all of the Ada
   --  specs defining __gnat_last_chance_handler, and being used by
   --  the GNATcoverage testsuite.  Even if the value is not modified
   --  in the body of Last_Chance_Handler, there may be procedures
   --  that look for this variable's symbol in the executable file,
   --  and which will cause errors if the symbol is not found.
   --
   --  In most programs, reaching  __gnat_last_chance_handler is
   --  unexpected. In such cases, the implementation of the body of
   --  Last_Chance_Handler that is linked into the program, contains
   --  code to indicate that it (Last_Chance_Handler) was called.
   --  The body that is linked in the case of a target system with the
   --  capability of producing output on Standard_Output, will generate
   --  a message using Put calls. If the target does not have output
   --  capability, this variable is set to a non-zero value in the body
   --  of Last_Chance_Handler, and the final value of
   --  Unexpected_Last_Chance_Call is inspected by external means
   --  after execution has completed (e.g. by a probe attached to the
   --  target board).
   --
   --  In programs for which a call to Last_Chance_Handler are expected,
   --  a version of the body will be used which neither outputs the
   --  error message, nor modifies the value of this variable.

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

end;
