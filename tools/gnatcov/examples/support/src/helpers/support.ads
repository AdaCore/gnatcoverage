package Support is

   --  First, common subprograms intended for either Ada or C tests.
   --  "assert" offers a more uniform termination behavior for C and
   --  Ada in case of failure.

   procedure Assert (Cond : Boolean);
   function Identity (X : Integer) return Integer;

   pragma Export (C, Assert, "assert");
   pragma Export (C, Identity, "identity");

   --  Extensions for Ada

   function Identity (B : Boolean) return Boolean;

   function Value (X : Integer) return Integer renames Identity;
   function Value (B : Boolean) return Boolean renames Identity;
end;
