separate (Pkg.Proc)
package body Nested is

   Register : Integer := 0;

   ---------
   -- Set --
   ---------

   procedure Set (Value : Integer) is
   begin
      Register := Value;
   end Set;

   ---------
   -- Get --
   ---------

   function Get return Integer is
   begin
      return Register;
   end Get;

end Nested;
