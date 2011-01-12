--  Provider of a functional AND decision featuring array slices in
--  conditions

with Blocks; use Blocks;

package FUOR is
   function Match_P_Or_S (Op : Block; P, S : Piece) return Boolean;
end;
