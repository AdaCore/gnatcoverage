--  Test driver for GOTO statements. It is supposed to execute GOTO statement
--  in an exception handler in the inner block statement, but it cannot do this
--  because the code is compiled with -gnatp.
--
--  Assertion and expectations in this test driver may need revising

with GOTO_Statements;      use GOTO_Statements;
with Support;              use Support;
procedure Test_GOTO_Statements_2 is
   Par1 : Integer;
   Par2 : Integer;
   Par3 : Integer;
   Par4 : Integer;

   Result : Integer;
begin
   Par1 := Integer'Last  / 200 - 3;
   Par2 := Integer'Last - 10;
   Par3 := 2;
   Par4 := 0;

   Result := Compute (Par1, Par2, Par3, Par4);

   Assert (Result = 0);
end Test_GOTO_Statements_2;

--# goto_statements.adb
-- /1inblock/   l+ 0
-- /1if/        l+ 0
-- /in1if/      l- s-
-- /1goto/      l- s-
-- /2block/     l+ 0
-- /2if/        ~l- ~s-
-- /in2if/      ~l- ~s-
-- /2goto/      ~l- ~s-
-- /after2goto/ ~l- ~s-
-- /afterblock/ l+ 0
-- /fin/        l+ 0
