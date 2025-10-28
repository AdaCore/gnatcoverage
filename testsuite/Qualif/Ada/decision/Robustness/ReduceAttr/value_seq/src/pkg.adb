pragma Ada_2022;

package body Pkg is

   -------------
   -- Compute --
   -------------

   function Compute (Inp : Arr_T; Op : Op_T) return Integer is
      function Reducer (Accum : Integer; Value : Integer) return Integer;

      -------------
      -- Reducer --
      -------------

      function Reducer (Accum : Integer; Value : Integer) return Integer is
        (if Op = Sum           -- # reducer_main
         then Accum + Value    -- # reducer_cont
         else Accum * Value);  -- # reducer_cont
   begin
      return [for Elt of Inp  -- # reduce_stmt
              when Elt /= 0 =>   -- # filter
              (if Op = Sum then Elt else abs Elt)]'Reduce  -- # map
                 (Reducer, (if Op = Sum then 0 else 1));  -- # reduce_dc
   end Compute;

end Pkg;
