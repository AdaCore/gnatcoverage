with Support; use Support;
package body More_IF_Statements is

   procedure Set_Prime_Number
     (Res : out Natural;
      Num :     Natural)
   is
   begin
      Res := 0;                      -- # prime

      if Num = 1 then                -- # ifprime
         Res := 2;                   -- # 1prime
      elsif Num = 2 then             -- # comp2prime
         Res := 3;                   -- # 2prime
      elsif Num = 3 then             -- # comp3prime
         Res := 5;                   -- # 3prime
      elsif Num = 4 then             -- # comp4prime
         Res := 7;                   -- # 4prime
      elsif Num = 5 then             -- # comp5prime
         Res := 11;                  -- # 5prime
      elsif Num = 6 then             -- # comp6prime
         Res := 13;                  -- # 6prime
      elsif Num = 7 then             -- # comp7prime
         Res := 17;                  -- # 7prime
      elsif Num = 8 then             -- # comp8prime
         Res := 19;                  -- # 8prime
      elsif Num = 9 then             -- # comp9prime
         Res := 23;                  -- # 9prime
      elsif Num = 10 then            -- # comp10prime
         Res := 29;                  -- # 10prime
      end if;

   end Set_Prime_Number;

   function Max_From_Two (Arg1, Arg2 : T) return T is
      Result : T; -- # declmax
   begin
      if Arg1 > Arg2 then            -- # max
         Result := Arg1;             -- # ifmax
      else
         Result := Arg2;             -- # elsemax
      end if;

      return Result;                 -- # max
   end Max_From_Two;

begin

   Global_Var := Identity (1);       -- # elab

   if Global_Var > 0 then            -- # elab
      Global_Var := Global_Var + 10; -- # gt0elab
   elsif Global_Var = 0 then         -- # notgt0elab
      Global_Var := Identity (100);  -- # eq0elabeq0
   end if;

end More_IF_Statements;

