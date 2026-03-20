--  Provider of an or-else decision evaluator with conditions involving
--  array aggregates

with Pools; use Pools;

package FUAND is
   function Invalid_Val_And_Loc (Op : Block) return Boolean;
   -- Whether Op.Val is "null" and Op.Loc are all Noloc_Id
end;
