--  Provider of an or-else decision evaluator with conditions involving
--  array aggregates

with Pools; use Pools;

package FUOR is
   function Invalid_Val_Or_Loc (Op : Block) return Boolean;
   -- Whether Op.Val is "null" or Op.Loc is all Noloc_Id
end;
