generic
   type T is digits <>;
package Services is
   procedure CheckN (N : T);
   pragma Precondition (N > 1.0);
end;


