with Ada.Finalization; use Ada.Finalization;

package body Foo is

   type Witness is new Ada.Finalization.Controlled with record
      Value : Integer;
   end record;

   procedure Adjust (X : in out Witness);
   procedure Finalize (X : in out Witness);

   procedure Adjust (X : in out Witness) is
   begin
      N_Adjust := N_Adjust + 1; -- # adjust
   end Adjust;

   procedure Finalize (X : in out Witness) is
   begin
      N_Finalize := N_Finalize + 1; -- # final
   end Finalize;

   function Make (V : Integer) return Witness is
   begin
      return (Controlled with V); -- # make
   end Make;

   function Val (X : Witness) return Integer is
   begin
      return X.Value; -- # val
   end Val;

   procedure Try (V : Integer) is
   begin
      if Val (Make (1)) = V or else Val (Make (2)) = V then -- # eval
         N_Then := N_Then + 1; -- # then
      else
         N_Else := N_Else + 1; -- # else
      end if;
   end Try;

end;
