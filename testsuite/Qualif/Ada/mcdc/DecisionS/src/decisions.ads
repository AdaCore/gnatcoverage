
--  this package contains tests to evaluate the treatment of
--  decisions composed of complex conditions like function
--  calls, operators, etc.
--  In addition, the behaviour is not controlled univocally by the
--  parameters, but involves a persistent state.

package Decisions is

   procedure FunctionCalls_And_Operators
     (I1, I2, I3, Max : Positive;
      Result : out Boolean);

private

   C1 : Boolean := False;
   C2 : Boolean := False;

end Decisions;
