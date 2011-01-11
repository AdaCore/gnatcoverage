--  Provider of a functional AND decision featuring array slices in
--  conditions

package FUAND is

   subtype Block  is String (1 .. 10);
   subtype Prefix is String (1 .. 3);
   subtype Suffix is String (1 .. 3);

   function Match (Op : Block; P : Prefix; S : Suffix) return Boolean;
end;
