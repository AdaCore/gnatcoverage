with Parser; use Parser;
procedure Test_Parser_0 is
begin
   Parser.Parse_Array ((0 => Tok_Close_Bracket));
end Test_Parser_0;

--# parser.adb
--  /append/  l- ## s-
--  /ex1/     l! ## dF-
--  /ex2/     l- ## s-
