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
      Elem : integer renames -- # st
        Input (if Item = First then 1 else 2);  -- # comp_expr
   begin
      return  -- # st
        [(if Use_Cache then Cached_Comp else Input) with delta  -- # base_expr_first
         (if Item = First then 1 else 2) =>                     -- # comp_expr
         (if Elem >= 0 then Elem else -Elem)];                  -- # value_expr_first
   end Copy_With_Abs_Update;
end Pkg;
