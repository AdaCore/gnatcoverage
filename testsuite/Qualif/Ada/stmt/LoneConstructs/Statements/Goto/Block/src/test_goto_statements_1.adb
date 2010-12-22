--  Test driver for GOTO statements. It executes GOTO statement in outer block
--  statement.

with GOTO_Statements;      use GOTO_Statements;
with Support;              use Support;
procedure Test_GOTO_Statements_1 is
   Par1 : Integer;
   Par2 : Integer;
   Par3 : Integer;
   Par4 : Integer;

   Result : Integer;
begin
   Par1 := (Integer'Last - 10) / 100;
   Par2 := 1;
   Par3 := 2;
   Par4 := 3;

   Result := Compute (Par1, Par2, Par3, Par4);

   Assert (Result = 1);
end Test_GOTO_Statements_1;

--# goto_statements.adb
-- /1inblock/   l+ 0
-- /1if/        l+ 0
-- /in1if/      l+ 0
-- /1goto/      l+ 0
-- /2block/     l- s-
-- /2if/        ~l- ~s-
-- /in2if/      ~l- ~s-
-- /2goto/      ~l- ~s-
-- /after2goto/ ~l- ~s-
-- /afterblock/ l- s-
-- /fin/        l+ 0
