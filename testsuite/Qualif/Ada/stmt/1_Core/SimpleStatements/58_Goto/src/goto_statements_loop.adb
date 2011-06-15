package body GOTO_Statements_Loop is

   procedure Update_Sample_For
     (Arg  : in out Sample;
      Par1 : in out Integer;
      Par2 : in out Integer)
   is
   begin
      for J in Arg'Range loop                                -- # 1loop
         if Arg (J) > 0 and then Arg (J) * Par1 = Par2 then  -- # 11if
            goto Found_Positive;                             -- # 11goto
         end if;

         if Arg (J) < 0 and then Arg (J) + Par1 = Par2 then  -- # 12if
            goto Found_Negative;                             -- # 12goto
         end if;

         Arg (J) := Arg (J) + J;                             -- # 1after2goto
      end loop;

      goto Not_Found;                                        -- # 13goto

      <<Found_Positive>>
      Arg (Arg'First) := 0;                                  -- # 1after3goto
      goto Fin;                                              -- # 14goto

      <<Found_Negative>>
      Arg (Arg'Last) := 0;                                   -- # 1after4goto
      goto Fin;                                              -- # 15goto

      <<Not_Found>>
      Par1 := 0;                                             -- # 1after5goto
      Par2 := 0;                                             -- # 1after5goto

      <<Fin>> null;                                          -- # 1fin
   end Update_Sample_For;

   procedure Update_Sample_While
     (Arg  : in out Sample;
      Par1 : in out Integer;
      Par2 : in out Integer)
   is
      J        : Natural;
      Last_Idx : Natural;

   begin
      J        := Arg'First;                                 -- # 2beforeloop
      Last_Idx := Arg'Last;                                  -- # 2beforeloop

      while J <= Last_Idx loop                               -- # 2loop
         if Arg (J) > 0 and then Arg (J) * Par1 = Par2 then  -- # 21if
            goto Found_Positive;                             -- # 21goto
         end if;

         if Arg (J) < 0 and then Arg (J) + Par1 = Par2 then  -- # 22if
            goto Found_Negative;                             -- # 22goto
         end if;

         Arg (J) := Arg (J) + J;                             -- # 2after2goto
          J := J + 1;                                        -- # 2after2goto
      end loop;

      goto Not_Found;                                        -- # 23goto

      <<Found_Positive>>
      Arg (Arg'First) := 0;                                  -- # 2after3goto
      goto Fin;                                              -- # 24goto

      <<Found_Negative>>
      Arg (Arg'Last) := 0;                                   -- # 2after4goto
      goto Fin;                                              -- # 25goto

      <<Not_Found>>
      Par1 := 0;                                             -- # 2after5goto
      Par2 := 0;                                             -- # 2after5goto

      <<Fin>> null;                                          -- # 2fin
   end Update_Sample_While;

   procedure Update_Sample
     (Arg  : in out Sample;
      Par1 : in out Integer;
      Par2 : in out Integer)
   is
      J        : Natural;
      Last_Idx : Natural;

   begin
      J        := Arg'First;                                 -- # 3beforeloop
      Last_Idx := Arg'Last;                                  -- # 3beforeloop

      loop                                                   -- # 3loop
         exit when J > Last_Idx;                            -- # 3exit

         if Arg (J) > 0 and then Arg (J) * Par1 = Par2 then  -- # 31if
            goto Found_Positive;                             -- # 31goto
         end if;

         if Arg (J) < 0 and then Arg (J) + Par1 = Par2 then  -- # 32if
            goto Found_Negative;                             -- # 32goto
         end if;

         Arg (J) := Arg (J) + J;                             -- # 3after2goto
          J := J + 1;                                        -- # 3after2goto
      end loop;

      goto Not_Found;                                        -- # 33goto

      <<Found_Positive>>
      Arg (Arg'First) := 0;                                  -- # 3after3goto
      goto Fin;                                              -- # 34goto

      <<Found_Negative>>
      Arg (Arg'Last) := 0;                                   -- # 3after4goto
      goto Fin;                                              -- # 35goto

      <<Not_Found>>
      Par1 := 0;                                             -- # 3after5goto
      Par2 := 0;                                             -- # 3after5goto

      <<Fin>> null;                                          -- # 3fin
   end Update_Sample;

end GOTO_Statements_Loop;

