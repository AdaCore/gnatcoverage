pragma Flare_0_1;
with Ada.Text_IO; use Ada.Text_IO;

with Pkg_Ex.Child; use Pkg_Ex.Child;

package body Pkg_Ex is

   procedure Exception_Finally (K : ErrorKind) is
   begin
      declare
         N : constant String := Raise_Decl (K = Decl);   -- # decl
      begin
         Put_Line (N);                                   -- # body
         Raise_Stmt (K /= Decl, K = CatchStmt);          -- # body
      exception
         when CaughtStmtError =>
            null;                                        -- # catch
      finally
         Put_Line ("THE END");                           -- # finally
      end;
   end Exception_Finally;

end Pkg_Ex;
