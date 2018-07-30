with Support; use Support;
with CASE_Statements; use CASE_Statements;
package body  More_CASE_Statements is

   procedure Set_Prime_Number
     (Res : out Natural;
      Num :     Natural)
   is
   begin

      case Num is                       -- # caseprime
         when 1 =>
            Res := 2;                   -- # 1prime
         when 2 =>
            Res := 3;                   -- # 2prime
         when 3 =>
            Res := 5;                   -- # 3prime
         when 4 =>
            Res := 7;                   -- # 4prime
         when 5 =>
            Res := 11;                  -- # 5prime
         when 6 =>
            Res := 13;                  -- # 6prime
         when 7 =>
            Res := 17;                  -- # 7prime
         when 8 =>
            Res := 19;                  -- # 8prime
         when 9 =>
            Res := 23;                  -- # 9prime
         when 10 =>
            Res := 29;                  -- # 10prime
         when others =>
            Res := 0;                   -- # othersprim
      end case;

   end Set_Prime_Number;

   function Adjust_Int_F (Arg : Integer) return Integer is
      Result : Integer;
   begin
      Result := Arg;                    -- # adjust

      case Result is                    -- # adjust
         when Int_Range_1 =>
            Result := Val_1;            -- # 1adjust
         when Int_Range_2 =>
            Result := Val_2;            -- # 2adjust
         when 100 | 200 | 300 =>
            Result := Result + Val_1;   -- # 100adjust
         when others =>
            Result := Result + Val_2;   -- # othersadjust
      end case;

      return Result;                    -- # adjust
   end Adjust_Int_F;

begin

   Global_Int := Identity (1);          -- # elab
   Global_Color := Adjust_Color (Green);-- # elab

   case Global_Int is                   -- # elab
      when Int_Range_1 =>
         Global_Color := Red;           -- # 1elab
      when 20 | 30 | 40  =>
         Global_Color := White;         -- # 2elab
      when others =>
         Global_Color := Black;         -- # otherselabint
   end case;

   case Global_Color is                 -- # elab
      when White .. Yellow =>
         Global_Int := 1;               -- # whiteelab
      when Blue | Black  =>
         Global_Int := 13;              -- # blueelab
      when others =>
         null;                          -- # otherselabcolor
   end case;

end More_CASE_Statements;

