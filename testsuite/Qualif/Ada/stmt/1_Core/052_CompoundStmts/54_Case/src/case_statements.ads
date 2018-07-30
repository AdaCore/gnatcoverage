--  Contains routines with different forms of CASE statements in different
--  kinds of program units

with CASE_Statements_Support; use CASE_Statements_Support;
package CASE_Statements is

   function Adjust_Color (Color : Colors) return Colors;
   --  Returns Colors'Next (Color) for White .. Brown. Returns White for Black.
   --  the body contains case statement with one choice in each alternative and
   --  without an OTHERS choice

   generic
      type T is range <>;
      Val1 : T;
      Val2 : T;
   procedure Adjust_Int_P
     (Val        : in out T;
      Arg1, Arg2 :        T);
   --  Changes the value of Val without any reasonable logic. The body contains
   --  CASE statement with OTHERS choice, with alternatives with one and with
   --  multiple choices, choices are values or ranges.
end CASE_Statements;

