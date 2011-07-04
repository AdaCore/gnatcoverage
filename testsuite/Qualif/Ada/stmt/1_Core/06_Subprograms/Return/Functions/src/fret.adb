package body Fret is

   procedure Proc1 (I : in Integer; J : out Integer) is
   begin
      J := I;                -- # proc1_1

      if J = 0 then          -- # proc1_1
         return;             -- # proc1_return
      end if;

      J := J * 2;            -- # proc1_after_return
   end Proc1;

   procedure Proc2 (I : in Integer; J : out Integer) is
   begin
      case I is              -- # proc2_start
         when 1 =>
            J := 2;          -- # proc2_1
            return;          -- # proc2_1
         when 2 =>
            J := 4;          -- # proc2_2
            return;          -- # proc2_2
         when 3 =>
            J := 7;          -- # proc2_3
            return;          -- # proc2_3
         when others =>
            null;            -- # proc2_others
      end case;

      J := 0;                -- # proc2_fin
   end Proc2;

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
