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
        (if Op = Sum                        -- # reducer_main
         then Accum + Value                 -- # reducer_cont
         else Integer'Max (Accum, Value));  -- # reducer_cont
   begin
      return Inp'Reduce                                      -- # reduce_st
        (Reducer, (if Op = Sum then 0 else Integer'First));  -- # reduce_dc
   end Compute;

end Pkg;
