--  Provider of an or-else decision evaluator with conditions involving
--  array aggregates

package FUOR is

   subtype Value is String (1 .. 4);
   subtype Pool  is String (1 .. 3);

   type Block is record
      Val : Value;
      Loc : Pool;
   end record;

   Noloc_Id : constant Character := '0';

   function Invalid (Op : Block) return Boolean;
   -- Whether Op.Val is "null" or Op.Loc are all Noloc_Id

end;
