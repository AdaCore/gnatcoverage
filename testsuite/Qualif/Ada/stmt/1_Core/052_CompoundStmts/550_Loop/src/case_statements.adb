package body CASE_Statements is

   function Adjust_Color (Color : Colors) return Colors is
   begin
      case Color is          -- # colorcase
         when White =>
            return Red;      -- # white
         when Red =>
            return Yellow;   -- # red
         when Yellow =>
            return Green;    -- # yellow
         when Green =>
            return Blue;     -- # green
         when Blue =>
            return Brown;    -- # blue
         when Brown =>
            return Black;    -- # brown
         when Black =>
            return White;    -- # black
      end case;
   end Adjust_Color;

   procedure Adjust_Int_P
     (Val        : in out T;
      Arg1, Arg2 :        T)
   is
   begin
      case Val is               -- # valcase
         when 1 =>
            Val := Val1;        -- # 1case
         when 2 | 22 =>
            Val := Val2;        -- # 2case
         when 4 .. 6 =>
            Val := Arg1;        -- # 4case
         when 7 | 10 .. 12 | 14 .. 16 | 19 =>
            Val := Arg2;        -- # 7case
         when others =>
            Val := 0;           -- # others
      end case;
   end Adjust_Int_P;

end CASE_Statements;

