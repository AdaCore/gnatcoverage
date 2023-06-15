package body Ops is
   procedure Compute
     (A, B, C : Integer;
      X, Y    : out Integer) is
   begin
      --  This test is about checking that gnatcov do not complain when
      --  multiple statements are on the same line as long as debug info slocs
      --  have column information.

      --  Currently, the compiler generates column information only for
      --  conditional branch instructions. We know that the full "mod"
      --  implementation involves branches, so we are sure that using such
      --  operations in both of our statements will generate column
      --  information.

      X := A mod B; Y := A mod C; -- # multi-stmts
   end Compute;
end Ops;
