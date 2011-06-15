--  This procedure contains nested block statements. The procedure performs
--  some completely meaningless computations...
procedure Nested_Block_Statements (I, J : in out Integer) is
begin
   declare
      Tmp1 : Integer := I;                    -- # outer_block
   begin
      I := J;                                 -- # outer_block
      J := Tmp1;                              -- # outer_block

      if I > 0 then                           -- # outer_block
          declare
             Tmp2 : Integer := (I + J) / 2;   -- # middle_block
          begin
             I := I + Tmp2;                   -- # middle_block
             J := J - Tmp2;                   -- # middle_block

             if J > 0 then                    -- # middle_block
                begin
                   I := I * 2;                -- # inner_block
                   J := J / 2;                -- # inner_block
                end;
             end if;

             I := I + 1;                      -- # middle_block
          end;
      end if;

      J := J + 1;                             -- # outer_block
   end;
end Nested_Block_Statements;
