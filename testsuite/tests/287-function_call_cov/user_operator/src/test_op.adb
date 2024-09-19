with Op;

--  LIMITATION
--  For now, gnatcov should not instrument calls to user-defined operators.
--  Check that this is true and that the proper warning is emitted.

procedure Test_Op is
begin
    Op;
end Test_Op;

--# op.adb
-- /fun/      l+ ## 0
-- /t_decl/   l+ ## 0
-- /op_call/  l? ## c?
-- /put_line/ l+ ## 0
