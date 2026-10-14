pragma Flare_0_1;

package Pkg_Ex is

   type ErrorKind is (Decl, Stmt, CatchStmt);

   procedure Exception_Finally (K : ErrorKind);
end Pkg_Ex;
