package body Decls_Pack_2 is

   procedure Local_Swap (V1, V2 : in out Access_Integer) is
      Tmp : Access_Integer := V1;  -- # local_swap
   begin
      V1  := V2;                   -- # local_swap
      V2  := Tmp;                  -- # local_swap
   end Local_Swap;

   function Local_Fun (I : Integer) return Access_Const_Integer is
      Result : Access_Const_Integer := new Integer'(I);  -- # decl
   begin

      if I < 0 then                -- # stmt
        Result := null;            -- # in_if
      end if;

      return Result;               -- # stmt
   end Local_Fun;


   package body Decls_Pack_Derived_Records_G is

      procedure Local_Swap (V1, V2 : in out Derived_Coordinate) is
         Tmp : Derived_Coordinate; -- # g1_local_swap
      begin
         if V1 /= V2 then          -- # g1_local_swap
            Tmp := V1;             -- # g1_if_local_swap
            V1  := V2;             -- # g1_if_local_swap
            V2  := Tmp;            -- # g1_if_local_swap
         end if;
      end Local_Swap;

      function Local_Fun (C1, C2 : Float) return Derived_Coordinate is
         Result : Derived_Coordinate;          -- # g1_decl
      begin

         if C1 > 0.0 and then C2 > 0.0 then    -- # g1_stmt
           Result.X := C1;                     -- # g1_in_if
           Result.Y := C2;                     -- # g1_in_if
         end if;

         return Result;                        -- # g1_stmt
      end Local_Fun;

   end Decls_Pack_Derived_Records_G;

   package body Decls_Pack_Private_G is

      procedure Local_Swap (V1, V2 : in out T_Private) is
         Tmp : T_Private := V1;              -- # g2_local_swap
      begin
         V1 := V2;                           -- # g2_local_swap
         V2 := Tmp;                          -- # g2_local_swap
      end Local_Swap;

      function Local_Fun (Arg : T_Private) return T_Private is
         Result : T_Private;                 -- # g2_decl
      begin

         case Get_Integer (Arg) is           -- # g2_stmt
            when 1 =>
               Result := Get_Private (100);  -- # g2_case1
            when 2 =>
               Result := T_Private_Zero;     -- # g2_case2
            when others =>
               null;                         -- # g2_case3
         end case;

         return Result;                      -- # g2_stmt
      end Local_Fun;

   end Decls_Pack_Private_G;

end Decls_Pack_2;
