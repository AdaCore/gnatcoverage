--  Common bits for functional decision evaluators featuring composite
--  aggregates in conditions

package Pools is

   subtype Value is String (1 .. 4);
   subtype Pool  is String (1 .. 3);

   type Block is record
      Val : Value;
      Loc : Pool;
   end record;

   Noloc_Id : constant Character := '0';

   Invalid_Val : Value := "null";
   Invalid_Loc : Pool  := (others => Noloc_Id);

   Valid_Val : Value := "abcd";
   Valid_Loc : Pool  := (others => 'C');

end;

