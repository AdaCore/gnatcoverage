package body Null_Statements is

   procedure Null_Proc_1 (I : Integer) is
   begin
      null;                        -- # emptynull1
   end Null_Proc_1;

   procedure Null_Proc_2 (B : Boolean) is
   begin
      null;                        -- # emptynull2
   end Null_Proc_2;

   procedure Adjust
     (Res : in out Integer;
      I, J : Integer)
   is
   begin
      case Res is                   -- # caseproc
         when 1 =>
            Res := Res * J;         -- # case1proc
         when 2 =>
            Res := Res + I;         -- # case2proc
         when others =>
            null;                   -- # casenullproc
      end case;

   end Adjust;

   function Adjust (Val : Integer; I, J : Integer) return Integer is
      Res : Integer := Val;
   begin
      case Res is                   -- # casefun
         when 1 =>
            Res := Res * J;         -- # case1fun
         when 2 =>
            Res := Res + I;         -- # case2fun
         when others =>
            null;                   -- # casenullfun
      end case;

      return Res;                   -- # casefun

   end Adjust;

   procedure Set_Max (Res : out Integer; I, J : Integer) is
   begin
      Res := I;           -- # maxfirst

      if Res > J then     -- # maxfirst
         goto Fin;        -- # maxif
      end if;

      Res := J;           -- # maxskip

      <<Fin>> null;       -- # maxnull
   end Set_Max;

   procedure Set_Min (Res : out Integer; I, J : Integer) is
   begin
      Res := I;           -- # minfirst

      if Res < J then     -- # minfirst
         goto Fin;        -- # minif
      end if;

      Res := J;           -- # minskip

      <<Fin>> null;       -- # minnull
   end Set_Min;

end Null_Statements;
