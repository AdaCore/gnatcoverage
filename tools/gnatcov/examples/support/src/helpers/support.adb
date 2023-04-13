
package body Support is

   procedure Assert (Cond : Boolean) is
   begin
      if not Cond then
         raise Program_Error;
      end if;
   end;

   function Identity (X : Integer) return Integer is
   begin
      return X;
   end;

   function Identity (B : Boolean) return Boolean is
   begin
      return B;
   end;
end;
