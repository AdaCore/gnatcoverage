pragma Ada_2012;

package TNocheck is
   pragma Assertion_Policy (Invariant => Ignore, Type_Invariant => Ignore);

   type Int (UB : Integer) is tagged record
      Value : Integer;
   end record;

   -- In type names, I stands for Invariant, TI for Type_Invariant,
   -- S for Simple, C for Complex.

   type SI is new Int with private;
   type STI is new Int with private;

private

   type SI is new Int with null record;
   pragma Invariant
     (SI, Check => SI.Value <= SI.UB); -- # si

   type STI is new Int with null record;
   pragma Type_Invariant
     (STI, Check => STI.Value <= STI.UB); -- # si

end;
