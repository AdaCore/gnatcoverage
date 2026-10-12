pragma Flare_0_1;

with Pkg_Ex;       use Pkg_Ex;
with Pkg_Ex.Child; use Pkg_Ex.Child;

procedure Test_Exception_Caught_Stmt is
begin
   Exception_Finally (CatchStmt);
exception
   when CaughtStmtError | StmtError | DeclError =>
      null;
end Test_Exception_Caught_Stmt;

--# pkg_ex.adb
-- /decl/      l+ ## 0
-- /body/      l+ ## 0
-- /finally/   l+ ## 0
