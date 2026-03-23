with State; use State;

package body Values is

   function One return Float is
   begin
      return 1.0; -- # stmt
   exception
      when Constraint_Error =>
         Dtick; -- # out
         return 2.0; -- # out
   end;

   function Value_Of (X : Integer) return Integer is
   begin
      return X; -- # stmt
   exception
      when Constraint_Error =>
         return 0;  -- # out
      when Program_Error =>
         return 66; -- # out
   end;

   function Value_Of_X return Integer is
   begin
      return X; -- # stmt
   exception
      when others => return 50; -- # out
   end;

   procedure Latch (V : Integer) is
   begin
      Tick; -- # stmt
      begin
         X := V; -- # stmt
      exception
         when Program_Error => Dtick; -- # out
      end;
   end;
end;
