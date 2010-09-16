--  This package contains library-level scalar variable object declaration with
--  explicit initialization expression and subprograms containing local
--  scalar variable object declarations with explicit initialization that are
--  expected to be covered only when subprograms are called.

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   Count : Natural := Identity (0);                -- # dcls

   procedure Local_Swap (I, J : in out Integer);

   function Local_Fun (Arg : Week_Day) return Week_Day;
   --  Returns the next week day. For Sunday returns Monday
end Decls_Pack;
