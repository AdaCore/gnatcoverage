pragma Flare_0_1;

package body Pkg_Ex.Child is

   function Raise_Decl (Fail : Boolean) return String is
   begin
      if Fail then
         raise DeclError;
      end if;
      return "";
   end Raise_Decl;

   procedure Raise_Stmt (Fail, Catch : Boolean) is
   begin
      if Fail then
         if Catch then
            raise CaughtStmtError;
         else
            raise StmtError;
         end if;
      end if;
   end Raise_Stmt;

end Pkg_Ex.Child;
