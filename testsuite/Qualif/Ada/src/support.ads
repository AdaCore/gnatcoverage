
package Support is
   procedure Assert (Cond : Boolean);
   function Identity (X : Integer) return Integer;
   function Identity (B : Boolean) return Boolean;

   function Value (X : Integer) return Integer renames Identity;
   function Value (B : Boolean) return Boolean renames Identity;
end;
