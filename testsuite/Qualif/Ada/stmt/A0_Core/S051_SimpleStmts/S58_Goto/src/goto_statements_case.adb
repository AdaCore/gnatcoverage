package body GOTO_Statements_Case is

   procedure Update
     (Arg  : in out Integer;
      Par1 : in out Integer;
      Par2 : in out Integer;
      Par3 :        Integer;
      Par4 :        Integer)
   is
   begin
      case Arg is                            -- # case
         when 1 | 3 | 5 =>                   -- # 1altcase

            if Arg < Par3 then               -- # 1if
               goto Label;                   -- # 1goto
            end if;

            if Arg > Par4 then               -- # 2if
               goto Fin;                     -- # 2goto
            end if;

            Par1 := 0;                       -- # in1altcase

         when My_Range =>

            if Arg < Par1 + Par2 then        -- # 3if
               goto Fin;                     -- # 3goto
            end if;

            Par2 := 0;                       -- # in2altcase

         when others =>                      -- # otherscase
            if Arg = Par1 + Par3 + Par4 then -- # 4if
               goto Fin;                     -- # 4goto
            end if;

            Par1 := Par3;                    -- # inotherscase

      end case;

      goto Fin;                              -- # 5goto

      <<Label>>
      Par2 := Par4;                          -- # after5goto

      <<Fin>> Arg := Par3 + Par4;            -- # fin
   end Update;

end GOTO_Statements_Case;
