package body Fret is

   function Fun1 (I : Integer) return Integer is
      Res : Integer := I;    -- # fun1_start
   begin
      if Res > 0 then        -- # fun1_start
         return Res;         -- # fun1_first_return
      end if;

      Res := Res + 10;       -- # fun_1_fin
      return Res;             -- # fun_1_fin
   end Fun1;

   function Fun2 (I : Integer) return Integer is
      Res : Integer := I;    -- # fun2_start
   begin

      if Res > 100 then      -- # fun2_start
         return Res + 2;     -- # fun2_1_return
      elsif Res > 10 then    -- # fun2_1_elsif
         return Res - 2;     -- # fun2_2_return
      elsif Res < 0 then     -- # fun2_2_elsif
         case Res is         -- # fun2_case
            when -1 =>
               return 1;     -- # fun2_3_return
            when -2 =>
               return 3;     -- # fun2_4_return
            when others =>
               null;         -- # fun2_others
         end case;
      end if;

      Res := Res * 2;        -- # fun_2_fin
      return Res;            -- # fun_2_fin
   end Fun2;

end Fret;
