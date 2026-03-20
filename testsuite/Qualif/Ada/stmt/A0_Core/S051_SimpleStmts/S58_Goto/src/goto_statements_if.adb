package body GOTO_Statements_If is

   procedure Update
     (Arg  : in out Integer;
      Par1 : in out Integer;
      Par2 : in out Integer;
      Par3 :        Integer;
      Par4 :        Integer)
  is
  begin
     if Arg = Par1 then               -- # if

        if Arg < Par3 then            -- # 1if
           goto Label;                -- # 1goto
        end if;

        if Arg > Par4 then            -- # 2if
           goto Fin;                  -- # 2goto
        end if;

        Par1 := 0;                    -- # inif

     elsif Arg > Par2 then            -- # elsif

        if Arg < Par1 + Par2 then     -- # 3if
           goto Fin;                  -- # 3goto
        end if;

        Par2 := 0;                    -- # inelsif

     else
        if Arg = Par3 + Par4 then     -- # 4if
           goto Fin;                  -- # 4goto
        end if;

        Par1 := Par3;                 -- # inelse

     end if;

     goto Fin;                        -- # 5goto

     <<Label>>
     Par2 := Par4;                    -- # after5goto

     <<Fin>> null;                    -- # fin
  end Update;

end GOTO_Statements_If;
