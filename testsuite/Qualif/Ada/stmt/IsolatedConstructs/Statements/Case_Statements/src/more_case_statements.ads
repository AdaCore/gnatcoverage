--  Contains routines with different forms of CASE statements in different
--  kinds of program units. The body contains CASE statements is an elaboration
--  statement sequence.

with CASE_Statements_Support; use CASE_Statements_Support;
package More_CASE_Statements is
   pragma Elaborate_Body;

   Global_Int  : Integer;
   Global_Color: Colors;

   procedure Set_Prime_Number
     (Res : out Natural;
      Num :     Natural);
   --  Sets Res to Nth prime number. If N > 10, sets Res to 0. The body
   --  contains CASE statements with many alternatives each containing one
   --  value as a choice, and OTHERS choice.

   generic
      Val_1 : Integer;
      Val_2 : Integer;
   function Adjust_Int_F (Arg : Integer) return Integer;
   --  Computes the result from Arg without any reasonable logic. The body
   --  contains CASE statement with OTHERS choice, with alternatives with
   --  choices that are subtype marks.

end More_CASE_Statements;

