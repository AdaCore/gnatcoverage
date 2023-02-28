with Lib_Loop_Statements;
with Support; use Support;

procedure Test_Init_Loop is
   X : Integer;
begin
   X := Lib_Loop_Statements.N_Ones;
   Assert (X = 0);
end;

--# lib_loop_statements.adb
--  /count/    l+ ## 0
--  /iter/     l+ ## 0
--  /test-inc/ l+ ## 0
--  /inc/      l- ## s-

--  /elab/      l+ ## 0
--  /test-init/ l+ ## 0
--  /init/      l- ## s-

