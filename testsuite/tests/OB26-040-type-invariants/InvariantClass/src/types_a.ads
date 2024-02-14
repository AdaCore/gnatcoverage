pragma Ada_2012;

package Types_A is
   pragma Assertion_Policy (Invariant => Check, Type_Invariant => Check);

   type Int (UB : Integer) is tagged private
     with Type_Invariant'Class => Valid (Int); -- # base

   function Valid (X : Int) return Boolean;

   -- In type names, I stands for Invariant, TI for Type_Invariant,
   -- S for Simple, C for Complex

   type SI is new Int with private;
   type CI is new Int with private;
   type STI is new Int with private;
   type CTI is new Int with private;

private

   type Int (UB : Integer) is tagged record
      Value : Integer := UB;
   end record;

   --  Invariant Aspects, Simple then Complex

   type SI is new Int with null record
      with Invariant => SI.Value > 0; -- # si

   type CI is new Int with null record
      with Invariant => CI.Value > 0; -- # ci

   --  Type_Invariant Aspects, Simple then Complex

   type STI is new Int with null record
      with Type_Invariant => STI.Value > 0; -- # sti

   type CTI is new Int with null record
      with Type_Invariant => CTI.Value > 0; -- # cti

end;
