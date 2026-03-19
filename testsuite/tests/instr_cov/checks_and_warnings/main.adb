--  If gnatcov does not remove this pragma, GNAT will warn about the redundant
--  with clause for Pkg.

pragma Warnings (On);
with Pkg;
with Pkg;

procedure Main is
   pragma Warnings (On);
   pragma Warnings ("a");
   pragma Style_Checks (On);
   pragma Style_Checks ("yg");

   --  The insertion of the witness call will make the following like exceed
   --  the 80 characters limit. Also, S is an unused variable, GNAT will warn
   --  about it.

   S : String := "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
begin
   Pkg.P;
end Main;
