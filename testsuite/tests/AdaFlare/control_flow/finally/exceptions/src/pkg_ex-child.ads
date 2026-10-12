pragma Flare_0_1;

package Pkg_Ex.Child is

   DeclError       : exception;
   StmtError       : exception;
   CaughtStmtError : exception;

   function Raise_Decl (Fail : Boolean) return String;

   procedure Raise_Stmt (Fail, Catch : Boolean);

end Pkg_Ex.Child;
