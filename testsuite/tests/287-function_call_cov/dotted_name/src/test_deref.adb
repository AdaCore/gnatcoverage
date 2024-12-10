with Deref;

--  LIMITATION
--  For now, gnatcov is unable to instrument calles as prefix of dotted names,
--  which includes explicit dereferences.

procedure Test_Deref is
begin
    Deref;
end Test_Deref;

--# deref.adb
-- /ok/     l+ ## 0
-- /deref1/ l? ## c?
-- /if/     l! ## dT-
-- /deref2/ l- ## s-,c?
