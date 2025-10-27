pragma Ada_2022;

package body Pkg is

   --------------------------
   -- Copy_With_Abs_Update --
   --------------------------

   function Copy_With_Abs_Update
     (Input     : Composite;
      Item      : Item_Type;
      Use_Cache : Boolean := False) return Composite
   is
      Elem : constant integer := -- # st
        (if Item = First then Input.First else Input.Last); -- # comp_expr
   begin
      --  We can't dynamically condition which component is used in a record
      --  delta aggregate, so in order to emulate the behavior we need to
      --  resort to two separate aggregates. This duplicates some decisions,
      --  only one will be covered by the driver meant to exercise the
      --  specific part of the aggregate.

      return  -- # st
        ((if Item = First                                              -- # comp_expr
          then ((if Use_Cache then Cached_Comp else Input) with delta  -- # base_expr_first
                First => (if Elem >= 0 then Elem else -Elem))          -- # value_expr_first
          else ((if Use_Cache then Cached_Comp else Input) with delta  -- # base_expr_last
                Last => (if Elem >= 0 then Elem else -Elem))));        -- # value_expr_last
   end Copy_With_Abs_Update;
end Pkg;
