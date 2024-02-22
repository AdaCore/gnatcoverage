--  CONFIG_PRAGMA

package Pkg is

   --  PRAGMA

   package Nested is
      procedure Set (Value : Integer);
      function Get return Integer;
   end Nested;

   procedure Proc;

end Pkg;
