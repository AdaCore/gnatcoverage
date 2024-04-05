package Pragmas is
   procedure Sum (X, Y, UB : Integer; R : out Integer);
   procedure Dif (X, Y, LB : Integer; R : out Integer);

   --  Number of calls to subprogram invoked through pragma
   --  Debug. Shoulds remain 0 as we're in a testcase intended to
   --  exercise situations where such pragmas are deactivated
   --  by a Check_Policy.
   Debug_Events : Integer := 0;
end;
