with Pkg;

procedure Test_Single is
begin
   Pkg.Print_If (True, True, True, "some message");
end Test_Single;

--# pkg.adb
--  /condition_1/ l+ ## mDcEval:"F - - -> FALSE (justification: J1)",
--                   +# mDcEval:"T F - -> FALSE (justification: J2)",
--                   +# mDcEval:"T T F -> FALSE (justification: J3)"
--  /condition_2/ l+ ## mDcEval:"F - -> FALSE (justification: J4)",
--                   +# mDcEval:"T F -> FALSE (justification: J5)"
--  /put_line/    l+ ## 0
