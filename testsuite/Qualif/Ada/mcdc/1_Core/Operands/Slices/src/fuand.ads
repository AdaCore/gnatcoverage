--  Provider of a functional and-then decision featuring array slices in
--  conditions

with Blocks; use Blocks;

package FUAND is
   function Match_P_And_S (Op : Block; P, S : Piece) return Boolean;
end;
