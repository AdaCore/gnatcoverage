package body Discrete_Subtype_Defs is

   function Some_Fun_1
     (I, J, K, L: Integer)
      return      Boolean
   is
      Result : Boolean;                                        -- # 1_local_dcl
      Tmp : array (I .. J) of Integer;                         -- # 1_local_dcl

   begin
      Result := True;                                          -- # 1_stmt

      for M in Tmp'Range loop                                  -- # 1_stmt
         Tmp (J) := M + K;                                     -- # 1_1_loop
      end loop;

      for M in Tmp'Range loop                                  -- # 1_stmt
         if Tmp (M) = L then                                   -- # 1_2_loop
            Result := False;                                   -- # 1_if
         end if;
      end loop;

      return Result;                                           -- # 1_stmt
   end Some_Fun_1;

   function Some_Fun_2
     (I, J, K, L : Integer)
      return       Boolean
   is
      Result : Boolean;                                        -- # 2_local_dcl
      type Tmp_Arr is array (Integer range I .. J) of Integer; -- # 2_local_dcl
      Tmp : Tmp_Arr;                                           -- # 2_local_dcl

   begin
      Result := True;                                          -- # 2_stmt

      for M in Tmp'Range loop                                  -- # 2_stmt
         Tmp (J) := M * K;                                     -- # 2_1_loop
      end loop;

      for M in Tmp'Range loop                                  -- # 2_stmt
         if Tmp (M) < L then                                   -- # 2_2_loop
            Result := False;                                   -- # 2_if
         end if;
      end loop;

      return Result;                                           -- # 2_stmt
   end Some_Fun_2;

end Discrete_Subtype_Defs;
