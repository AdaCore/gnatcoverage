--  Provider of an or-else decision evaluator which features conditions
--  involving integer modulo computations

package FUOR is
   type Modop is record
      X, Y : Integer;
   end record;

   function Mod0_Or (OpA, OpB : Modop) return Boolean;
   --  Whether OpA.X mod OpA.Y or OpB.X mod OpB.Y is null
end;



