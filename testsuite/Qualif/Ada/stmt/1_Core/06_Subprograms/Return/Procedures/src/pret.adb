package body Pret is

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

end Pret;
