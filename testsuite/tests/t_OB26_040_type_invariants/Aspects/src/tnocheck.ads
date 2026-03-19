pragma Ada_2012;

package Tnocheck is
   pragma Assertion_Policy (Invariant => Disable, Type_Invariant => Disable);

   type Int (UB : Integer) is tagged record
      Value : Integer := UB;
   end record;

   -- In type names, I stands for Invariant, TI for Type_Invariant,
   -- S for Simple, C for Complex

   type SI is new Int with private;
   type STI is new Int with private;

private

   --  Invariant and Type_Invariant Aspects, Simple

   type SI is new Int with null record
      with Invariant => SI.Value <= SI.UB; -- # si

   type STI is new Int with null record
      with Type_Invariant => STI.Value > 0; -- # si

end;
