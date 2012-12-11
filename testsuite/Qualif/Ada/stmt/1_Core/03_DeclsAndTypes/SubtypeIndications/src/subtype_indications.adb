package body Subtype_Indications is

   function Get_Var_String
     (Len : Index;
      Data : String)
      return Var_String
   is
      subtype Result_Subtype is Var_String (Len);      -- # 1_local_dcl
      Result : Result_Subtype;                         -- # 1_local_dcl
   begin
      Result.Data := Data;                             -- # 1_stmt
      return Result;                                   -- # 1_stmt
   end Get_Var_String;

   function Simple_Sort (V : Vector) return Vector is
      Result : Vector (V'Range);                       -- # 2_local_dcl
      First       : Integer := Result'First;           -- # 2_local_dcl
      Last        : Integer := Result'Last;            -- # 2_local_dcl
      Current_Max : Integer := First;                  -- # 2_local_dcl
      Tmp         : Integer;                           -- # 2_bare_dcl
   begin
      Result := V;                                     -- # 2_stmt

      for J in First .. Last loop                      -- # 2_stmt
         Current_Max := J;                             -- # 2_stmt

         for K in J .. Last loop                       -- # 2_stmt
            if Result (K) > Result (Current_Max) then  -- # 2_stmt
               Current_Max := K;                       -- # 2_stmt
            end if;
         end loop;

         Tmp                  := Result (J);           -- # 2_stmt
         Result (J)           := Result (Current_Max); -- # 2_stmt
         Result (Current_Max) := Tmp;                  -- # 2_stmt

      end loop;

      return Result;                                   -- # 2_stmt
   end Simple_Sort;

   function Some_Fun
     (Len : Integer;
      S   : String)
      return Integer
   is
      type Some_Record is record                       -- # 3_local_dcl
         C : Var_String (Len);                         -- # 3_local_dcl
      end record;                                      -- # 3_local_dcl

      Tmp : Some_Record;                               -- # 3_local_dcl
      Result : Integer := 0;                           -- # 3_local_dcl

   begin
      if S'length = Len then                           -- # 3_stmt
         Tmp.C := (Len, S);                            -- # 3_stmt

         for J in 1 .. Len loop                        -- # 3_stmt
            if Tmp.C.Data (J) > '0' then               -- # 3_stmt
               Result := Result + 1;                   -- # 3_stmt
            end if;
         end loop;
      end if;

      return Result;                                   -- # 3_stmt
   end Some_Fun;

end Subtype_Indications;
