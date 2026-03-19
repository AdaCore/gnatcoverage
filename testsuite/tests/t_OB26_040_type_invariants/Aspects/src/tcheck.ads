pragma Ada_2012;

package Tcheck is
   pragma Assertion_Policy (Invariant => Check, Type_Invariant => Check);

   type Int (UB : Integer) is tagged record
      Value : Integer := UB;
   end record;

   -- In type names, I stands for Invariant, TI for Type_Invariant,
   -- S for Simple, C for Complex

   type SI is new Int with private;
   type CI is new Int with private;
   type STI is new Int with private;
   type CTI is new Int with private;

private

   --  Invariant Aspects, Simple then Complex

   type SI is new Int with null record
      with Invariant => SI.Value <= SI.UB; -- # si

   type CI is new Int with null record
      with Invariant =>
        CI.Value <= CI.UB and then CI.Value > 0; -- # ci

   --  Type_Invariant Aspects, Simple then Complex

   type STI is new Int with null record
      with Type_Invariant => STI.Value > 0; -- # si

   type CTI is new Int with null record
      with Type_Invariant =>
        CTI.Value <= CTI.UB and then CTI.Value > 0; -- # ci

end;
