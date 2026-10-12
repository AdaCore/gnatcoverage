pragma Flare_0_1;

with Pkg_Ex;       use Pkg_Ex;
with Pkg_Ex.Child; use Pkg_Ex.Child;

procedure Test_Exception_Decl is
begin
   Exception_Finally (Decl);
exception
   when CaughtStmtError | StmtError | DeclError =>
      null;
end Test_Exception_Decl;

--# pkg_ex.adb
-- /body/      l- ## s-
-- /finally/   l- ## s-
-- /catch/     l- ## s-
--
-- %tags: block
-- /decl/      l- ## s-
