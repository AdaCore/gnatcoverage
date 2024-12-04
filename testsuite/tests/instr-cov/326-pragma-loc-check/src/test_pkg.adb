with Pkg;

procedure Test_Pkg is
begin
   Pkg.Wrong_Do_Dump;
   pragma Annotate (Xcov, Dump_Buffers);
end Test_Pkg;

--  st can only be covered if both buffer annotations have been ignored

--# pkg.adb
--
--  /st/ l+ ## 0
