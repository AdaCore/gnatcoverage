
--  This is an Ada program implementing an example pre-testsuite hook, invoked
--  before the sequence of actual tests starts.

with Ada.Text_IO; use Ada.Text_IO;

procedure Pre_Testsuite is
begin
   Put_Line ("Pre Testsuite example hook running !!");
end;
