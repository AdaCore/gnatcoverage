with Pkg; use Pkg;
with Pkh; use Pkh;

procedure Test_Elab is
   Dummy : Boolean;
   pragma Volatile (Dummy);
begin
   Dummy := Bar (Foo);
end Test_Elab;

--# pkg.ads
--
--  /decl/      l+ ## 0
--  /elab_spec/ l! ## eT-

--# pkg.adb
--
--  /decl/      l+ ## 0
--  /fun/       l+ ## 0
--  /elab_body/ l! ## eT-

--# pkh.adb
--
--  /elab_spec/ l. ## 0

--# pkh.adb
--
--  /elab_fun/ l! ## eT-
