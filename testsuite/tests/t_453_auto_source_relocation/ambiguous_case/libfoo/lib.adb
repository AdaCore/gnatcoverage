with Interfaces.C; use Interfaces.C;

package body Lib is
   function Square (A : Natural) return Natural is
      function Square_C (A : unsigned) return unsigned;
      pragma Import (C, Square_C, "square");
   begin
      return Natural (Square_C (unsigned (A)));
   end Square;
end;
