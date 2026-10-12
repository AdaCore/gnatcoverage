pragma Flare_0_1;

with Pkg_Ex;       use Pkg_Ex;
with Pkg_Ex.Child; use Pkg_Ex.Child;

procedure Test_Exception_Stmt is
begin
   Exception_Finally (Stmt);
exception
   when CaughtStmtError | StmtError | DeclError =>
      null;
end Test_Exception_Stmt;

--# pkg_ex.adb
-- /decl/      l+ ## 0
-- /body/      l+ ## 0
-- /finally/   l+ ## 0
-- /catch/     l- ## s-
